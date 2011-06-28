import Test.Hspec
import Test.Hspec.Core
import Test.Hspec.Runner
import Test.Hspec.Formatters
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
  let (actions, filenames, errors) = getOpt RequireOrder options args
      opts = (foldl (.) id actions) startOptions
      formatter = getFormatter (formatterName opts) (useColor opts)
  if help opts
   then printHelp
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
     specsList <- mapM (getSpecsFromFile opts) filenames
     results <- runSpecs opts formatter (concat specsList)
     exitWith . toExitCode . success $ results




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





getSpecs :: String -> String -> IO [IO Spec]
getSpecs filename contents = do
  let matches = (contents =~ "[^ ]+ *:: *(Test\\.Hspec\\.)?Specs") :: [[String]]
  specs <- mapM (\ m -> loadSpecs filename (getName $ head m) :: IO [IO Spec]) matches
  return $ concat specs

getIOSpecs :: String -> String -> IO [IO Spec]
getIOSpecs filename contents = do
  let matches = (contents =~ "[^ ]+ *:: *IO +(Test\\.Hspec\\.)?Specs") :: [[String]]
  specs <- sequence =<< mapM (\ m -> loadSpecs filename (getName $ head m) :: IO (IO [IO Spec])) matches
  return $ concat specs

getMonadicSpecs :: String -> String -> IO [IO Spec]
getMonadicSpecs filename contents = do
  let matches = (contents =~ "[^ ]+ *:: *Test\\.Hspec\\.Monadic\\.Specs") :: [[String]]
  specs <- mapM (\ m -> loadSpecs filename (getName $ head m) :: IO Monadic.Specs) matches
  specs2 <- mapM Monadic.runSpecM specs
  return $ concat specs2

getMonadicIOSpecs :: String -> String -> IO [IO Spec]
getMonadicIOSpecs filename contents = do
  let matches = (contents =~ "[^ ]+ *:: *IO +Test\\.Hspec\\.Monadic\\.Specs") :: [[String]]
  specs <- sequence =<< mapM (\ m -> loadSpecs filename (getName $ head m) :: IO (IO Monadic.Specs)) matches
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
            Nothing -> output opts == "stdout"
            Just x  -> x

withHandle :: String -> (Handle -> IO a) -> IO a
withHandle "stdout" f = f stdout
withHandle "stderr" f = f stderr
withHandle filename f = bracket (openFile filename WriteMode)
                                (hClose)
                                (f)




data Options = Options {
                formatterName :: String,
                output :: String,
                specificExample :: Maybe String,
                color :: Maybe Bool,
                help :: Bool }

startOptions :: Options
startOptions = Options {
                formatterName = "specdoc",
                output = "stdout",
                specificExample = Nothing,
                color = Nothing,
                help = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "f" ["format"]
      (ReqArg (\x s -> s { formatterName = map toLower x }) "FORMAT")
      "Specifies what format to use for output.\n\
      \By default the specdoc format is used.\n\
      \FORMAT can be silent, progress, specdoc, or failed_examples."
  , Option "o" ["output"]
      (ReqArg (\x s -> s { output = x }) "FILE_NAME")
      "Specifies the file to use for output.\n\
      \By default output is directed to stdout.\n\
      \FILE_NAME can be stdout or stderr for those handles."
--  , Option "e" ["example"]
--      (ReqArg (\x s -> s { specificExample = Just x }) "REGEX")
--      "Only execute examples with a matching description.\n\
--      \By default all examples are executed."
  , Option "c" ["color"]
      (ReqArg (\ x s -> s { color = Just (map toLower x == "true") }) "TRUE|FALSE")
      "Force output to have or not have red and green color.\n\
      \By default color is only used when output is directed to stdout."
  , Option "h?" ["help"]
      (NoArg (\ s -> s { help = True }))
      "Display this help."
  ]

helpInfo :: String
helpInfo = "hspec searches through the specified files and runs any \
           \top level items with a type of `Specs` or `IO Specs`. \
           \Monadic specs must be fully qualified, list-based specs \
           \may be qualified or not. \n\n\
           \usage: hspec [OPTIONS] file1 [file2...]"

printHelp :: IO ()
printHelp = putStrLn $ usageInfo helpInfo options
