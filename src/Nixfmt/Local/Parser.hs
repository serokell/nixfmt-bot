{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Local.Parser where

import Text.Megaparsec as M hiding (many)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, string, space1, space)

type Parser = Parsec Void Text

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

parseFormatted :: Text -> Text -> Parser [Text]
parseFormatted beginBlock endBlock = many $ M.try $ toText <$> do
  _ <- manyTill anySingle (string beginBlock)
  _ <- char '\n'
  manyTill anySingle (M.try $ space *> string endBlock)

replaceOldFragments :: Text -> Text -> [Text] -> Parser Text
replaceOldFragments _ _ [] = toText <$> manyTill anySingle eof
replaceOldFragments beginBlock endBlock (txt:txts) = do
    oldBefore <- toText <$> manyTill anySingle (string beginBlock)
    _ <- char '\n'
    _ <- manyTill anySingle (string endBlock)
    _ <- char '\n'
    newBelow <- M.try $ replaceOldFragments beginBlock endBlock txts
    return $ oldBefore <> txt <> "\n" <> newBelow
