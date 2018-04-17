{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Cats.Parser
  ( allImportDeclarations
  ) where

import           Control.Applicative ((<|>))
import           Data.Functor        (($>))

import           Cats.Lexer
import qualified Text.Parsec         as Parsec

allImportDeclarations = Parsec.try (Parsec.many tryImport) <|> return []
  where
    tryImport =
      Parsec.try $
      Parsec.try importDeclaration <|> (Parsec.anyChar *> tryImport)

-- from: https://www.ecma-international.org/ecma-262/8.0/index.html#sec-imports
-- ImportDeclaration:
--  import ImportClause FromClause;
--  import ModuleSpecifier;
importDeclaration =
  (reserved "import" *> importClause *> fromClause) <|>
  (reserved "import" *> moduleSpecifier)

-- ImportClause:
--  ImportedDefaultBinding
--  NameSpaceImport
--  NamedImports
--  ImportedDefaultBinding, NameSpaceImport
--  ImportedDefaultBinding, NamedImports
data ImportClause
  = Default ImportedDefaultBinding
  | NameSpace NameSpaceImport
  | Named NamedImports
  | DefaultWithNameSpace ImportedDefaultBinding
                         NameSpaceImport
  | DefaultWithNamed ImportedDefaultBinding
                     NamedImports

data ImportedDefaultBinding =
  ImportedDefaultBinding String

data NameSpaceImport =
  NameSpaceImport String

data NamedImports =
  NamedImports [String]

importClause =
  Parsec.choice
    (map
       Parsec.try
       [ DefaultWithNameSpace <$> importedDefaultBinding <*>
         (comma *> nameSpaceImport)
       , DefaultWithNamed <$> importedDefaultBinding <*> (comma *> namedImports)
       , Default <$> importedDefaultBinding
       , NameSpace <$> nameSpaceImport
       , Named <$> namedImports
       ])

-- ImportedDefaultBinding:
--  ImportedBinding
importedDefaultBinding = ImportedDefaultBinding <$> importedBinding

-- NameSpaceImport:
--  * as ImportedBinding
nameSpaceImport =
  NameSpaceImport <$>
  (lexeme (Parsec.char '*') *> lexeme (Parsec.string "as") *> importedBinding)

-- NamedImports:
--  {}
--  {ImportsList}
--  {ImportsList,}
namedImports =
  NamedImports <$>
  Parsec.choice
    (map
       Parsec.try
       [ braces (importsList <* Parsec.optional comma)
       , braces whiteSpace $> return []
       ])

-- FromClause:
--  from ModuleSpecifier
fromClause = lexeme (Parsec.string "from") *> moduleSpecifier

-- ImportsList:
--  ImportSpecifier
--  ImportsList, ImportSpecifier
importsList = importSpecifier `Parsec.sepEndBy` comma

-- ImportSpecifier:
--  ImportedBinding
--  IdentifierName as ImportedBinding
importSpecifier =
  Parsec.choice
    (map
       Parsec.try
       [ identifier *> lexeme (Parsec.string "as") *> importedBinding
       , importedBinding
       ])

-- ModuleSpecifier:
--  StringLiteral
moduleSpecifier = stringLiteral -- NOTE: this is the only thing I care about

-- ImportedBinding:
--  BindingIdentifier[~Yield, ~Await]
importedBinding = identifier
