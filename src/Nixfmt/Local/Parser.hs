{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Local.Parser
       ( parseFiles
       , parseDiff
       , parseFormatted
       , replaceOldFragments) where

import Text.Megaparsec (Parsec, anySingle, eof, manyTill, noneOf)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, string, space1, space)

type Parser = Parsec Void Text

-- | Parse output of git diff
parseDiff :: Parser [(Int, Int)]
parseDiff = many (M.try parseDiff')

parseDiff' :: Parser (Int, Int)
parseDiff' = do
  _ <- many (noneOf ("@@":: String ))
  _ <- many (noneOf ("+" :: String))
  fstLine <- char '+' *> decimal
  numberOfLines <- (char ',' *> decimal) <|> (return 1)
  _ <- space1 *> string "@@"
  return (fstLine, numberOfLines)

-- | Get changed pieces of code.
parseFormatted :: Text -> Text -> Parser [Text]
parseFormatted beginBlock endBlock = many $ M.try $ toText <$> do
  _ <- manyTill anySingle (string beginBlock)
  _ <- char '\n'
  manyTill anySingle (M.try $ space *> string endBlock)

-- | Get original text and replace changed pieces of code by formatted ones.
replaceOldFragments :: Text -> Text -> [Text] -> Parser Text
replaceOldFragments _ _ [] = toText <$> manyTill anySingle eof
replaceOldFragments beginBlock endBlock (txt:txts) = do
    oldBefore <- toText <$> manyTill anySingle (string beginBlock)
    _ <- char '\n'
    _ <- manyTill anySingle (string endBlock)
    _ <- char '\n'
    newBelow <- M.try $ replaceOldFragments beginBlock endBlock txts
    return $ oldBefore <> txt <> "\n" <> newBelow

parseFiles :: Text -> Parser [Text]
parseFiles path = many $ M.try $ do
    c    <- manyTill anySingle (space1)
    file <- toText <$> manyTill anySingle ((char '\n' >> return ()) <|> eof)
    return $ if c == "A" || c == "M"
             then path <> file
             else ""
