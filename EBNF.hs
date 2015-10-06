module EBNF
( Symbol(Term, NTerm, Many, Option)
, NTermName
, ProductionRHS
, Grammar
, symbolName
) where
 
import qualified Data.Map.Strict as Map

data Symbol = Term String |
              NTerm String |
              Option [Symbol] |
              Many [Symbol] 
              deriving (Show)

type NTermName = String
type ProductionRHS = [Symbol]
type Grammar = Map.Map NTermName [ProductionRHS]

symbolName (Term name) = name
symbolName (NTerm name) = name
symbolName (Many []) = undefined
symbolName (Many (s:_)) = symbolName s
symbolName (Option []) = undefined
symbolName (Option (s:_)) = symbolName s

