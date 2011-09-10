module Main where

import Test.Hspec
import Test.Hspec.Core
import Test.Hspec.Runner
import Test.Hspec.Formatters
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit
import qualified Test.Hspec.Monadic
import qualified Test.Hspec.Monadic as Monadic
import Text.Regex.Posix
import System.Environment
import System.IO
import System.Directory
import System.FilePath
import System.Exit
import System.Plugins
import System.Console.GetOpt
import Control.Exception
import Data.Char (toLower,toUpper,isAlphaNum)


debug :: Options -> String -> IO ()
debug opts msg = if verbose opts then putStrLn (" [verbose] " ++ msg) else return ()

main :: IO ()
main = do
  result <- getArgs >>= runWithArgs
  case result of
    Left results  -> exitWith . toExitCode . success $ results
    Right message -> putStrLn message >> exitFailure


runWithArgs :: [String] -> IO (Either Specs String)
runWithArgs args = do
  let (targets, errors, opts) = getOptions args
  if help opts
    then printHelp >> return (Left [])
    else if not $ null errors
    then return $ Right $ unlines errors
    else runWithTargetsAndOptions targets opts

runWithTargetsAndOptions :: [String] -> Options -> IO (Either Specs String)
runWithTargetsAndOptions targets opts = do
    allSpecsFound <- getAllSpecs targets opts
    if null allSpecsFound
      then return $ Right "no valid *.hs files found\n"
      else do
        result <- runSpecsWithOptions allSpecsFound opts
        case result of
          Left results -> do
            updateLastRunFile opts results
            return $ Left results
          Right message -> return $ Right message

getAllSpecs :: [String] -> Options -> IO Specs
getAllSpecs targets opts = do
  filenames <- getFileNamesToSearchFromTargets (getTargets opts targets)
  specs <- getAllSpecsFromFiles filenames opts
  return $ includeSelfSpecs specs opts

runSpecsWithOptions :: Specs -> Options -> IO (Either Specs String)
runSpecsWithOptions allFoundSpecs opts = do
  result <- filterSpecs allFoundSpecs opts
  case result of
    Left specsToRun   -> runSpecs opts specsToRun >>= return . Left
    Right noneMessage -> return $ Right noneMessage

runSpecs :: Options -> Specs -> IO Specs
runSpecs opts specs = withHandle (output opts) work
  where work h = hHspecWithFormat formatter h specs
        formatter = getFormatter (formatterToUse opts) (useColor opts)

updateLastRunFile :: Options -> [Spec] -> IO ()
updateLastRunFile opts results = do
  let fileName = lastRunFile opts
      failedExamples = unlines [ requirement s | s <- results, isFailure (result s)]
  isFile <- doesFileExist fileName
  case (failedCount results, isFile) of
    (0, True) -> do
      debug opts $ "removing " ++ fileName ++ " since there are no failed examples"
      removeFile fileName
    (0, False) -> debug opts $ "skipping " ++ fileName ++ " since file doesn't exist and there are no failed examples"
    (_, _   ) -> do
      debug opts $ "writing failed examples to " ++ fileName
      writeFile fileName failedExamples

getFileNamesToSearchFromTargets :: [String] -> IO [String]
getFileNamesToSearchFromTargets names = do
    files <- mapM getFileNamesToSearchFromTarget names
    return $ concat files

getFileNamesToSearchFromTarget :: String -> IO [String]
getFileNamesToSearchFromTarget pathName = do
  isFile <- doesFileExist pathName
  isDir <- doesDirectoryExist pathName
  if isFile
   then if takeExtension pathName == ".hs"
        then return [pathName]
        else return []
   else if isDir
   then do
    names <- getDirectoryContents pathName
    let validNames = map (combine pathName) $ filter ((/='.').head) names
    getFileNamesToSearchFromTargets validNames
   else fail $ pathName ++ " is not a valid *.hs file or directory name"


-- Parsing haskell files
hasModuleHeader :: String -> Bool
hasModuleHeader contents = getFirstWord contents == "module" -- lame

getFirstWord :: String -> String
getFirstWord xs@('{':'-':_) = getFirstWord (removeBlockComment xs 0)
getFirstWord xs@('-':'-':_) = getFirstWord (removeLineComment xs)
getFirstWord    (' ':xs)    = getFirstWord xs
getFirstWord    ('\t':xs)   = getFirstWord xs
getFirstWord    ('\n':xs)   = getFirstWord xs
getFirstWord     xs         = takeWhile isAlphaNum xs

removeBlockComment :: String -> Int -> String
removeBlockComment ('-':'}':xs) d
  | d == 1    = xs
  | otherwise = removeBlockComment xs (d-1)
removeBlockComment ('{':'-':xs) d = removeBlockComment xs (d+1)
removeBlockComment (_:xs)       d = removeBlockComment xs d
removeBlockComment []           _ = fail "bad block comment"

removeLineComment :: String -> String
removeLineComment ('\n':xs)    = xs
removeLineComment ('-':'-':xs) = removeLineComment xs
removeLineComment (_:xs)       = removeLineComment xs
removeLineComment []           = fail "bad line comment"



-- Loading specs from files
getAllSpecsFromFiles :: [String] -> Options -> IO [Spec]
getAllSpecsFromFiles filenames opts = fmap concat $ mapM (getAllSpecsFromFile opts) filenames

getAllSpecsFromFile :: Options -> String -> IO Specs
getAllSpecsFromFile opts filename = do
  contents <- readFile filename
  if contents =~ "^module Main where$"
   then do
     debug opts $ "skipping " ++ filename ++ " because it is a Main module and those can't be dynamically loaded."
     return []
   else if hasModuleHeader contents
   then do
     debug opts $ "seaching " ++ filename
     (s1,n1) <- getSpecsFromFile filename contents
     (s2,n2) <- getIOSpecsFromFile filename contents
     (ms1,n3) <- getMonadicSpecsFromFile filename contents
     (ms2,n4) <- getMonadicIOSpecsFromFile filename contents
     mapM_ (debug opts) (map ("  "++) (concat $ [n1, n2, n3, n4]))
     return (s1 ++ s2 ++ ms1 ++ ms2)
   else do
      let justName = takeFileName filename
          cleanName = dropExtension $ toUpper (head justName) : tail justName
      debug opts $ "skipping " ++ filename ++ " because only library modules (like \"module " ++ cleanName ++ " where\") can be dynamically loaded."
      return []

getName :: String -> String
getName = takeWhile (`notElem`" :")

getDeclariations :: String -> String -> [String]
getDeclariations pattern contents = map head matches
  where matches = (contents =~ pattern) :: [[String]]


getSpecDeclariations :: String -> [String]
getSpecDeclariations = getDeclariations "^[^ ]+ *:: *(Test\\.Hspec\\.)?Specs\\w*$"

getSpecsFromFile :: String -> String -> IO (Specs,[String])
getSpecsFromFile filename contents = do
  let names = getSpecDeclariations contents
  specs <- mapM (\ n -> loadSpecsFromFile filename (getName n) :: IO Specs) names
  return (concat specs, names)


getIOSpecDeclariations :: String -> [String]
getIOSpecDeclariations = getDeclariations "^[^ ]+ *:: *IO +(Test\\.Hspec\\.)?Specs\\w*$"

getIOSpecsFromFile :: String -> String -> IO (Specs,[String])
getIOSpecsFromFile filename contents = do
  let names = getIOSpecDeclariations contents
  specs <- sequence =<< mapM (\ n -> loadSpecsFromFile filename (getName n) :: IO (IO Specs)) names
  return (concat specs, names)

getMonadicSpecDeclariations :: String -> [String]
getMonadicSpecDeclariations = getDeclariations "^[^ ]+ *:: *Test\\.Hspec\\.Monadic\\.Specs\\w*$"

getMonadicSpecsFromFile :: String -> String -> IO (Specs,[String])
getMonadicSpecsFromFile filename contents = do
  let names = getMonadicSpecDeclariations contents
  specs <- mapM (\ n -> loadSpecsFromFile filename (getName n) :: IO Monadic.Specs) names
  return (concatMap Monadic.runSpecM specs, names)


getMonadicIOSpecDeclariations :: String -> [String]
getMonadicIOSpecDeclariations = getDeclariations "^[^ ]+ *:: *IO +Test\\.Hspec\\.Monadic\\.Specs\\w*$"

getMonadicIOSpecsFromFile :: String -> String -> IO (Specs,[String])
getMonadicIOSpecsFromFile filename contents = do
  let names = getMonadicIOSpecDeclariations contents
  specs <- sequence =<< mapM (\ n -> loadSpecsFromFile filename (getName n) :: IO (IO Monadic.Specs)) names
  return (concatMap Monadic.runSpecM specs, names)

loadSpecsFromFile :: String -> String -> IO a
loadSpecsFromFile filename specName = do
  status <- makeAll filename []
  obj    <- case status of
              MakeSuccess _ o -> return o
              MakeFailure e   -> mapM_ putStrLn e >> fail "can't make"
  mv <- load obj ["."] [] specName
  case mv of
    LoadFailure msgs -> putStrLn (unwords msgs) >> fail "can't load"
    LoadSuccess _ v  -> return v



-- excluding from loaded specs
filterSpecs :: [Spec] -> Options -> IO (Either [Spec] String)
filterSpecs allSpecsFound opts = do
  (matchingSpecs, noneMessage) <- filterByExampleOption opts allSpecsFound
  specsToRun <- filterByRerunOption opts matchingSpecs
  return $ if null specsToRun then Right noneMessage else Left specsToRun

filterByExampleOption :: Options -> Specs -> IO (Specs, String)
filterByExampleOption opts specs =
  case specificExample opts of
    Nothing      -> return (specs, "no specs found")
    Just pattern -> do
        debug opts $ "only running descriptions matching \"" ++ pattern ++ "\""
        let s = filterSpecsByRegex pattern specs
        return (s, "no specs matching \"" ++ pattern ++ "\" found")

filterByRerunOption :: Options -> Specs -> IO Specs
filterByRerunOption opts specs
  | lastRunOption opts == RunFailed = do
    isFile <- doesFileExist (lastRunFile opts)
    if isFile
      then do
        failedDescriptions <- readFile (lastRunFile opts)
        return $ filterExactSpecs (lines failedDescriptions) specs
      else return specs
  | otherwise = return specs

filterSpecsByRegex :: String -> Specs -> Specs
filterSpecsByRegex pattern = filter p
  where p s = requirement s =~ pattern

filterExactSpecs :: [String] -> Specs -> Specs
filterExactSpecs ok = filter p
  where p s = requirement s `elem` ok



-- getting specific options
getTargets :: Options -> [String] -> [String]
getTargets opts xs
  | runSelfSpecs opts = xs
getTargets _ [] = ["."]
getTargets _ xs = xs

getFormatter :: String -> (Bool -> Formatter)
getFormatter "silent" = silent
getFormatter "progress" = progress
getFormatter "specdoc" = specdoc
getFormatter "failed_examples" = failed_examples
getFormatter _ = specdoc

useColor :: Options -> Bool
useColor opts = case color opts of
            Nothing -> output opts `elem` ["stdout","stderr"]
            Just x  -> x

withHandle :: String -> (Handle -> IO a) -> IO a
withHandle "stdout" f = f stdout
withHandle "stderr" f = f stderr
withHandle filename f = bracket (openFile filename WriteMode)
                                (hClose)
                                (f)

includeSelfSpecs :: Specs -> Options -> Specs
includeSelfSpecs specs opts
  | runSelfSpecs opts = specs ++ Monadic.runSpecM allSpecs
  | otherwise         = specs



-- getting Options
getOptions :: [String] -> ([String], [String], Options)
getOptions args = (filenames, errors, opts)
    where (actions, filenames, errors) = getOpt RequireOrder options args
          opts = (foldl (.) id actions) startOptions

data LastRunOption = RunAll | RunFailed
  deriving Eq

data Options = Options {
                formatterToUse :: String,
                output :: String,
                specificExample :: Maybe String,
                color :: Maybe Bool,
                help :: Bool,
                runSelfSpecs :: Bool,
                verbose :: Bool,
                lastRunFile :: String,
                lastRunOption :: LastRunOption }

startOptions :: Options
startOptions = Options {
                formatterToUse = "specdoc",
                output = "stdout",
                specificExample = Nothing,
                color = Nothing,
                help = False,
                runSelfSpecs = False,
                verbose = False,
                lastRunFile = ".hspecLastRun",
                lastRunOption = RunAll }

trim :: String -> String
trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

options :: [OptDescr (Options -> Options)]
options =
  [ Option "f" ["format"]
      (ReqArg (\x s -> s { formatterToUse = trim $ map toLower x }) "FORMAT")
      "Specifies what format to use for output.\n\
      \By default the specdoc format is used.\n\
      \FORMAT can be silent, progress, specdoc, or failed_examples."
  , Option "o" ["output"]
      (ReqArg (\x s -> s { output = trim $ x }) "FILE_NAME")
      "Specifies the file to use for output.\n\
      \By default output is directed to stdout.\n\
      \FILE_NAME can be stdout or stderr for those handles."
  , Option "e" ["example"]
      (ReqArg (\x s -> s { specificExample = Just x }) "REGEX")
      "Only execute examples with a matching description.\n\
      \By default all examples are executed."
  , Option "c" ["color"]
      (ReqArg (\ x s -> s { color = Just (map toLower (trim x) == "true") }) "TRUE|FALSE")
      "Force output to have or not have red and green color.\n\
      \By default color is only used when output is directed to stdout."
  , Option "" ["runfile"]
      (ReqArg (\ x s -> s { lastRunFile = x }) "FILE_NAME")
      "Use a specific file to log the last run results. This is read when using the --rerun option.\n\
      \By default the file \".hspecLastRun\" logs the last run results."
  , Option "r" ["rerun"]
      (ReqArg (\ x s -> let opt = case map toLower (trim x) of
                                    "all"    -> RunAll
                                    "failed" -> RunFailed
                                    _        -> error $ "unsupported run option \"" ++ x ++ "\""
                        in s { lastRunOption = opt }) "RERUNOPT")
      "Rerun a specific subset of specs. This looks at the last run file specified by --runfile.\n\
      \RERUNOPT can be \"all\" or \"failed\".\n\
      \By default the last run file is ignored and all specs are run."
  , Option "h?" ["help"]
      (NoArg (\ s -> s { help = True }))
      "Display this help."
  , Option "v" ["verbose"]
      (NoArg (\ s -> s { verbose = True }))
      "Display detailed information about what hspec is doing."
  , Option "" ["specs"]
      (NoArg (\ s -> s { runSelfSpecs = True }))
      "Include the specs for the hspec command line runner itself. When used, the target list \n\
      \will not default to the current directory. A non-empty target list will still be \n\
      \searched though."
  ]

helpInfo :: String
helpInfo = "hspec searches through files or folders and runs any top level declarations with \
           \a type of `Specs` or `IO Specs`. Monadic specs must be fully qualified, \
           \list-based specs may be qualified or not. You can specify specific *.hs \
           \files or directories to search through or let hspec search the current \
           \directory tree for specs to run.\n\n\
           \usage: hspec [OPTIONS] [TARGET_LIST]"

printHelp :: IO ()
printHelp = putStrLn $ usageInfo helpInfo options





-- Specs
allSpecs :: Test.Hspec.Monadic.Specs
allSpecs =
    Monadic.describe "hspec command line runner" $ do

      Monadic.it "searches target files for specs and runs them"
        (True)

      Monadic.describe "options" $ do
        let test :: (Eq a, Show a) => String -> a -> (Options -> a) -> IO ()
            test arg expected func = HUnit.assertEqual arg expected (func opts)
              where (_, _, opts) = getOptions (words arg)

        Monadic.it "accepts -h, -?, and --help to get help"
          (do
           test "-h" True help
           test "-?" True help
           test "--help" True help
           test "" False help)

        Monadic.it "accepts -c or --color to set weather or not to color the output"
          (do
           test "-c false" (Just False) color
           test "-c true" (Just True) color
           test "--color false" (Just False) color
           test "--color true" (Just True) color
           test "" Nothing color)

        Monadic.it "accepts -f or --format and a formatter name to use"
          (do
           test "-f progress" "progress" formatterToUse
           test "--format progress" "progress" formatterToUse
           test "" "specdoc" formatterToUse)

        Monadic.it "accepts -o or --output and a filename to output to"
          (do
           test "-o foo.txt" "foo.txt" output
           test "--output foo.txt" "foo.txt" output
           test "" "stdout" output)

        Monadic.it "accepts -o or --output and stdout or stderr to output to"
          (do
           test "-o stderr" "stderr" output
           test "--output stdout" "stdout" output)

        Monadic.it "accepts --specs to display it's own specs"
          (do
           test "--specs" True runSelfSpecs
           test "" False runSelfSpecs)

        Monadic.it "accepts -v or --verbose to display extra details"
          (do
           test "-v" True verbose
           test "--verbose" True verbose
           test "" False verbose)

        Monadic.it "accepts one or more targets to search"
          (let args = ["a", "b"]
               (files, _, _) = getOptions args
           in HUnit.assertEqual (unwords args) args files)

        Monadic.it "defaults to the current directory tree if no targets are specified and not running --specs"
          (HUnit.assertEqual "" ["."] (getTargets startOptions []))

      Monadic.describe "an hspec target" $ do

        Monadic.it "can be a file name to run all specs in that file"
          (do
          filenames <- getFileNamesToSearchFromTargets ["hspec.hs"]
          HUnit.assertEqual "hspec.hs" ["hspec.hs"] filenames)

        Monadic.it "can be a directory name to run all specs in that directory tree"
          (do
          filenames <- getFileNamesToSearchFromTargets ["."]
          HUnit.assertBool "hspec.hs" ("./hspec.hs" `elem` filenames))

      Monadic.describe "the --format option" $ do

        let test :: String -> (Bool -> Formatter) -> IO ()
            test arg expected = HUnit.assertEqual arg (formatterName $ expected False) (formatterName $ getFormatter (formatterToUse opts) $ False)
              where (_, _, opts) = getOptions ["--format", arg]

        Monadic.it "allows the \"silent\" formatter"
          (test "silent" silent)

        Monadic.it "allows the \"progress\" formatter"
          (test "progress" progress)

        Monadic.it "allows the \"specdoc\" formatter"
          (test "specdoc" specdoc)

        Monadic.it "allows the \"failed_examples\" formatter"
          (test "failed_examples" failed_examples)

        Monadic.it "uses \"specdoc\" by default"
          (test "" specdoc)

      Monadic.describe "the --color option" $ do

        let test :: String -> String -> Bool -> IO ()
            test colorArg outputArg expected = HUnit.assertEqual (unwords args) expected (useColor opts)
              where args = if colorArg == "" then ["-o", outputArg] else ["-c", colorArg, "-o", outputArg]
                    (_, _, opts) = getOptions args

        Monadic.it "allows \"true\" to force color output"
          (test "true" "stdout" True)

        Monadic.it "allows \"false\" to force non-color output"
          (test "false" "stdout" False)

        Monadic.it "by default, uses color when outputing to stdout"
          (test "" "stdout" True)

        Monadic.it "by default, uses color when outputing to stderr"
          (test "" "stderr" True)

        Monadic.it "by default, doesn't color when outputing to a file"
          (test "" "foo.txt" False)

      Monadic.describe "searching for specs to run" $ do

        let test :: String -> [String] -> IO ()
            test contents expected = HUnit.assertEqual contents expected decs
              where decs = concat [ getSpecDeclariations contents,
                                    getIOSpecDeclariations contents,
                                    getMonadicSpecDeclariations contents,
                                    getMonadicIOSpecDeclariations contents ]

        Monadic.it "finds top level definitions of type \"Specs\""
          (test "\ntest :: Specs\ntest = undefined\n"
                ["test :: Specs"])

        Monadic.it "finds top level definitions of type \"IO Specs\""
          (test "\ntest :: IO Specs\ntest = undefined\n"
                ["test :: IO Specs"])

        Monadic.it "finds top level definitions of type \"Test.Hspec.Specs\""
          (test "\ntest :: Test.Hspec.Specs\ntest = undefined\n"
                ["test :: Test.Hspec.Specs"])

        Monadic.it "finds top level definitions of type \"IO Test.Hspec.Specs\""
          (test "\ntest :: IO Test.Hspec.Specs\ntest = undefined\n"
                ["test :: IO Test.Hspec.Specs"])

        Monadic.it "finds top level definitions of type \"Test.Hspec.Monadic.Specs\""
          (test "\ntest :: Test.Hspec.Monadic.Specs\ntest = undefined\n"
                ["test :: Test.Hspec.Monadic.Specs"])

        Monadic.it "finds top level definitions of type \"IO Test.Hspec.Monadic.Specs\""
          (test "\ntest :: IO Test.Hspec.Monadic.Specs\ntest = undefined\n"
                ["test :: IO Test.Hspec.Monadic.Specs"])

        Monadic.it "finds top level definitions after single line comments"
          (test "\n-- single line comment\ntest :: IO Specs\ntest = undefined\n"
                ["test :: IO Specs"])

        Monadic.it "finds top level definitions after multi line comments"
          (test "\n{- multi\n line\n comment -}\ntest :: IO Specs\ntest = undefined\n"
                ["test :: IO Specs"])

        Monadic.it "finds top level definitions after pragma style comments"
          (test "{-# OPTIONS_GHC -fno-warn-unused-binds #-}\n\
                \{-# LANGUAGE QuasiQuotes #-}\n\
                \test :: Specs\n\
                \test = undefined\n"
                ["test :: Specs"])

        Monadic.it "finds module declaration" $
          hasModuleHeader "{-# OPTIONS_GHC -fno-warn-unused-binds #-}\n\
                \{-# LANGUAGE QuasiQuotes #-}\n\
                \module  main ()  where \n\
                \test :: Specs\n\
                \test = undefined\n"
