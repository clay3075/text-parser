module Parser (
    ParserType (FileParser, DirectoryParser, StringParser),
    parse,
    parseFile,
    parseDirectory,
    Parser (Parser, id, name, rules, fileExt)
) where

import System.Directory --(doesFileExist, getDirectoryContents)
import System.FilePath.Posix --(replaceDirectory)
import Control.Monad

import Replace -- (ReplaceExact, ReplaceAllAfter, applyReplaceRules)

type InputFilePath = FilePath
type OutputFilePath = FilePath
data ParserType = FileParser (InputFilePath, OutputFilePath) | DirectoryParser (InputFilePath, OutputFilePath) | StringParser String
data Parser = Parser { id :: String, name :: String, rules :: [ReplaceRule], fileExt :: String}

parse :: Parser -> ParserType -> IO [FilePath]
parse parser parserType = case parserType of
                    StringParser contents           -> return [parseString parser contents]
                    FileParser (input, output)      -> parseFile parser parserType
                    DirectoryParser (input, output) -> parseDirectory parser parserType

parseString :: Parser -> String -> String
parseString parser content = applyReplaceRules (rules parser) content

parseFile :: Parser -> ParserType -> IO [FilePath]
parseFile parser (FileParser (inputPath, outputPath)) = do
    fileContents <- readFile inputPath
    writeFile outputPath (parseString parser fileContents)
    return [outputPath]

parseDirectory :: Parser -> ParserType -> IO [FilePath]
parseDirectory parser (DirectoryParser (inputPath, outputPath)) = do
    paths <- getDirectoryContents inputPath
    filePaths <- filterM (\path -> doesFileExist (inputPath </> path)) paths
    list <- mapM (\path -> parseFile parser (FileParser (inputPath </> path, getOutputPath path))) filePaths
    return (concat list)

    where 
        getOutputPath path = replaceExtension (replaceDirectory path outputPath) (fileExt parser)