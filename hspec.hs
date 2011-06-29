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
import System.Exit
import System.Plugins
import System.Console.GetOpt
import Control.Exception
import Data.Char (toLower,toUpper,isAlphaNum)
import Data.List (intersperse)

main :: IO ()
main = do
  args <- getArgs
  let (filenames, errors, opts, formatter) = getOptions args
  if help opts
   then printHelp
   else if runSelfSpecs opts
   then do
     Monadic.hspecX allSpecs
     return ()
   else if not $ null errors
   then do
     mapM_ putStrLn errors
     printHelp
     exitFailure
   else if null filenames
   then do
     putStrLn "must specify at least one input file\n"
     printHelp
     exitFailure
   else do
     runSpecsFromFiles filenames opts formatter

runSpecsFromFiles :: [String] -> Options -> Formatter -> IO ()
runSpecsFromFiles filenames opts formatter = do
     specsList <- mapM (getSpecsFromFile opts) filenames
     results <- runSpecs opts formatter (concat specsList)
     exitWith . toExitCode . success $ results

getOptions args = (filenames, errors, opts, formatter)
    where (actions, filenames, errors) = getOpt RequireOrder options args
          opts = (foldl (.) id actions) startOptions
          formatter = getFormatter (formatterToUse opts) (useColor opts)



getSpecsFromFile :: Options -> String -> IO [IO Spec]
getSpecsFromFile opts filename = do
  contents <- readFile filename
  if hasModuleHeader contents
   then do
     s1 <- getSpecs filename contents
     s2 <- getIOSpecs filename contents
     ms1 <- getMonadicSpecs filename contents
     ms2 <- getMonadicIOSpecs filename contents
     return (s1 ++ s2 ++ ms1 ++ ms2)
   else do
      let cleanName = takeWhile (/='.') $ toUpper (head filename) : tail filename
      putStrLn $ filename ++ " must have a module header (like \"module " ++ cleanName ++ " where\")"
      fail $ "invalid file " ++ filename


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
removeBlockComment (x:xs)       d = removeBlockComment xs d
removeBlockComment []           d = fail "bad block comment"

removeLineComment :: String -> String
removeLineComment ('\n':xs)    = xs
removeLineComment ('-':'-':xs) = removeLineComment xs
removeLineComment (_:xs)       = removeLineComment xs
removeLineComment []           = fail "bad line comment"



getDeclerations :: String -> String -> [String]
getDeclerations pattern contents = map head matches
  where matches = (contents =~ pattern) :: [[String]]


getSpecDeclerations :: String -> [String]
getSpecDeclerations = getDeclerations "^[^ ]+ *:: *(Test\\.Hspec\\.)?Specs\\w*$"

getSpecs :: String -> String -> IO [IO Spec]
getSpecs filename contents = do
  let names = getSpecDeclerations contents
  specs <- mapM (\ n -> loadSpecs filename (getName n) :: IO [IO Spec]) names
  return $ concat specs


getIOSpecDeclerations :: String -> [String]
getIOSpecDeclerations = getDeclerations "^[^ ]+ *:: *IO +(Test\\.Hspec\\.)?Specs\\w*$"

getIOSpecs :: String -> String -> IO [IO Spec]
getIOSpecs filename contents = do
  let names = getIOSpecDeclerations contents
  specs <- sequence =<< mapM (\ n -> loadSpecs filename (getName n) :: IO (IO [IO Spec])) names
  return $ concat specs


getMonadicSpecDeclerations :: String -> [String]
getMonadicSpecDeclerations = getDeclerations "^[^ ]+ *:: *Test\\.Hspec\\.Monadic\\.Specs\\w*$"

getMonadicSpecs :: String -> String -> IO [IO Spec]
getMonadicSpecs filename contents = do
  let names = getMonadicSpecDeclerations contents
  specs <- mapM (\ n -> loadSpecs filename (getName n) :: IO Monadic.Specs) names
  specs2 <- mapM Monadic.runSpecM specs
  return $ concat specs2


getMonadicIOSpecDeclerations :: String -> [String]
getMonadicIOSpecDeclerations = getDeclerations "^[^ ]+ *:: *IO +Test\\.Hspec\\.Monadic\\.Specs\\w*$"

getMonadicIOSpecs :: String -> String -> IO [IO Spec]
getMonadicIOSpecs filename contents = do
  let names = getMonadicIOSpecDeclerations contents
  specs <- sequence =<< mapM (\ n -> loadSpecs filename (getName n) :: IO (IO Monadic.Specs)) names
  specs2 <- mapM Monadic.runSpecM specs
  return $ concat specs2


runSpecs :: Options -> Formatter -> [IO Spec] -> IO [Spec]
runSpecs opts formatter specs = withHandle (output opts) work
  where work h = hHspecWithFormat formatter h (return specs)

getName :: String -> String
getName = takeWhile (`notElem`" :")




loadSpecs :: String -> String -> IO a
loadSpecs filename name = do
  status <- makeAll filename []
  obj    <- case status of
              MakeSuccess _ o -> return o
              MakeFailure e   -> mapM_ putStrLn e >> fail "can't make"
  mv <- load obj ["."] [] name
  case mv of
    LoadFailure msgs -> putStrLn (unwords msgs) >> fail "can't load"
    LoadSuccess _ v  -> return v


--getSpecificSpecs :: String -> [IO Spec] -> IO [IO Spec]
--getSpecificSpecs pattern specs = sequence specs >>= return . map return . filter p
--  where p s = requirement s =~ pattern



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




data Options = Options {
                formatterToUse :: String,
                output :: String,
                specificExample :: Maybe String,
                color :: Maybe Bool,
                help :: Bool,
                runSelfSpecs :: Bool }

startOptions :: Options
startOptions = Options {
                formatterToUse = "specdoc",
                output = "stdout",
                specificExample = Nothing,
                color = Nothing,
                help = False,
                runSelfSpecs = False }

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
--  , Option "e" ["example"]
--      (ReqArg (\x s -> s { specificExample = Just x }) "REGEX")
--      "Only execute examples with a matching description.\n\
--      \By default all examples are executed."
  , Option "c" ["color"]
      (ReqArg (\ x s -> s { color = Just (map toLower (trim x) == "true") }) "TRUE|FALSE")
      "Force output to have or not have red and green color.\n\
      \By default color is only used when output is directed to stdout."
  , Option "h?" ["help"]
      (NoArg (\ s -> s { help = True }))
      "Display this help."
  , Option "" ["specs"]
      (NoArg (\ s -> s { runSelfSpecs = True }))
      "Display the specs for the hspec command line runner itself."
  ]

helpInfo :: String
helpInfo = "hspec searches through the specified files and runs any \
           \top level items with a type of `Specs` or `IO Specs`. \
           \Monadic specs must be fully qualified, list-based specs \
           \may be qualified or not. \n\n\
           \usage: hspec [OPTIONS] file1 [file2...]"

printHelp :: IO ()
printHelp = putStrLn $ usageInfo helpInfo options




allSpecs :: Test.Hspec.Monadic.Specs
allSpecs = do
       commandLineSpecs
       formatSpecs
       colorSpecs
       findingSpecsSpecs

commandLineSpecs :: Test.Hspec.Monadic.Specs
commandLineSpecs = let test :: (Eq a, Show a) => String -> a -> (Options -> a) -> IO ()
                       test arg expected func = HUnit.assertEqual arg expected (func opts)
                        where (_, _, opts, _) = getOptions (words arg)
  in Monadic.describe "hspec command line runner" $ do
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

        Monadic.it "requires one or more filenames to search"
          (let args = ["a", "b"]
               (files, _, _, _) = getOptions args
           in HUnit.assertEqual (unwords args) args files)


formatSpecs :: Test.Hspec.Monadic.Specs
formatSpecs = let test :: String -> (Bool -> Formatter) -> IO ()
                  test arg expected = HUnit.assertEqual arg (expected False) (getFormatter (formatterToUse opts) $ False)
                   where (_, _, opts, _) = getOptions ["--format", arg]
  in Monadic.describe "the --format option" $ do
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


colorSpecs :: Test.Hspec.Monadic.Specs
colorSpecs = let test :: String -> String -> Bool -> IO ()
                 test colorArg outputArg expected = HUnit.assertEqual (unwords args) expected (useColor opts)
                  where args = if colorArg == "" then ["-o", outputArg] else ["-c", colorArg, "-o", outputArg]
                        (_, _, opts, _) = getOptions args
  in Monadic.describe "the --color option" $ do
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


findingSpecsSpecs :: Test.Hspec.Monadic.Specs
findingSpecsSpecs = let test :: String -> [String] -> IO ()
                        test contents expected = HUnit.assertEqual contents expected decs
                         where decs = concat [ getSpecDeclerations contents,
                                               getIOSpecDeclerations contents,
                                               getMonadicSpecDeclerations contents,
                                               getMonadicIOSpecDeclerations contents ]
  in Monadic.describe "hspec" $ do
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
