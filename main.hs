import System.Environment
import Parser
import qualified VBToJavascriptParser
import Control.Monad
import Data.List

main = do
    args <- getArgs
    case processInput args of
        Just x  -> x
        Nothing -> putStrLn "Failed" >>= \c -> return [c]

printHelp :: String -> IO ()
printHelp progName = do
    putStrLn ""
    putStrLn (progName ++ " <parser> <parse_type> <content_to_parse> <output_file_path>")
    putStrLn "<parser>: "
    printAvailableParsers
    putStrLn "<parse_type>:       -file, -dir, -str"
    putStrLn "<content_to_parse>: file_path, dir_path, string (ignores output_file_path)"
    putStrLn ""
    putStrLn ""

processInput :: [String] -> Maybe (IO [()])
processInput [parser, "-file", inputPath, outputPath] = getParser parser >>= \p -> Just (parse p (FileParser (inputPath, outputPath))) >>= \res -> printOutput res
processInput [parser, "-dir",  inputPath, outputPath] = getParser parser >>= \p -> Just (parse p (DirectoryParser (inputPath, outputPath))) >>= \res -> printOutput res
processInput [parser, "-str",  content]               = getParser parser >>= \p -> Just (parse p (StringParser content)) >>= \res -> printOutput res
processInput ["-help"]                                = Just (getProgName >>= printHelp >>= \c -> return [c])
processInput otherwise                                = Just (putStrLn "Improper input. Try using -help" >>= \c -> return [c])

printOutput :: IO [String] -> Maybe (IO [()])
printOutput output = do Just (output >>= (\o -> mapM putStrLn o))

availableParsers :: [Parser]
availableParsers = [ VBToJavascriptParser.parser ]

printAvailableParsers :: IO ()
printAvailableParsers = do
    let printParserInfo parser = putStrLn $ "          " ++ unwords [ "(" ++ Parser.id parser ++ ")", Parser.name parser] 
    mapM printParserInfo availableParsers
    return ()

getParser :: String -> Maybe Parser
getParser key = find (\p -> Parser.id p == key) availableParsers