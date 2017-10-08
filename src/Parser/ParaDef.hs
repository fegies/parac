module Parser.ParaDef where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

paraDef :: LanguageDef st
paraDef = emptyDef
            {
                commentStart = "/*",
                commentEnd = "*/",
                commentLine = "//",
                nestedComments = False,
                identStart = lower,
                identLetter = letter <|> char '_' <|> digit,
                reservedOpNames = ["->", "=>"],
                reservedNames = ["module", "import", "function", "while", "for", "return", "if", "else", "type", "class", "instance", "data", "enum", "var"]
            }