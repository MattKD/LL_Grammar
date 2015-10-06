module LLGrammar 
( Symbol(Term, NTerm)
, symbolName
, grammarToStr
, grammarToStr_
, grammarToStrs
, grammarToStrs_
, ebnfToCFG 
, subAllHeadNTerms
, subAllHeadNTerms_
, fixAllLeftRecur
, fixAllLeftRecur_
, fixAllCommonPrefix
, fixAllCommonPrefix_
, NTermName
, Prefix
, ProductionRHS
, PrefixPair
, Grammar
) where
  
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.List
import Data.Char
import qualified EBNF as EBNF

data Symbol = Term String | 
              NTerm String deriving (Show, Eq)

symbolName :: Symbol -> String
symbolName (Term name) = name
symbolName (NTerm name) = name

type NTermName = String
type Prefix = [Symbol]
type ProductionRHS = [Symbol]
type PrefixPair = (Prefix, [ProductionRHS])
type NTermSet = Set.Set NTermName
type Grammar = Map.Map NTermName [ProductionRHS]

-- Create a new name based on passed in one that isn't in Grammar.
-- To resolve name collisions, "'" are appended to name.
checkNTermName :: Grammar -> NTermName -> NTermName
checkNTermName grammar name = 
  if Map.member name grammar then
    checkNTermName grammar (name ++ "'")
  else name
 
-- Transform a Grammar to a more readable string.
-- NTerms should be lowercase, Terms uppercase or inside quotes,
-- and empty RHS productions as \NULL.
-- Example:
-- nterm ::= 
--   'termA' nterm2 TermB |
--   \NULL
grammarToStr :: Grammar -> String
grammarToStr grammar = grammarToStr_ grammar (Map.keys grammar)

grammarToStr_ :: Grammar -> [NTermName] -> String
grammarToStr_ grammar nterms = concat $ concat $ grammarToStrs_ grammar nterms 

-- Transform a Grammar with list of NTerm names to a list of list of
-- Strings: list of the lines in a NTermName and its RHS Productions
grammarToStrs :: Grammar -> [[String]]
grammarToStrs grammar = grammarToStrs_ grammar (Map.keys grammar)

grammarToStrs_ :: Grammar -> [NTermName] -> [[String]]
grammarToStrs_ grammar nterms = foldr gts [] nterms where
  gts :: NTermName -> [[String]] -> [[String]]
  gts nterm strss = let
    str = nterm ++ " ::= \n\t"
    rhss = Maybe.fromJust $ Map.lookup nterm grammar
    strs = [str] ++ (gts2 rhss) 
    in strs : strss

  gts2 :: [ProductionRHS] -> [String]
  gts2 [] = undefined
  gts2 (rhs:[]) = let 
    str = gts3 rhs ++ "\n"
    in [str]
  gts2 (rhs:rhss) = let
    str = gts3 rhs ++ "|\n\t"
    in [str] ++ gts2 rhss

  gts3 :: ProductionRHS -> String
  gts3 [] = "\\NULL "
  gts3 symbols = concat $ map ((++ " ") . symbolStr) symbols

  symbolStr (Term name) = 
    if isUpper (head name) then name 
    else '\'' : name ++ "'"
  symbolStr (NTerm name) = name

-- Convert Extended Backus-Naur Form to a Context-Free Grammar.
ebnfToCFG :: EBNF.Grammar -> Grammar
ebnfToCFG ebnfProds = cfgProds where
  acc = (Map.map (const []) ebnfProds)
  cfgProds = Map.foldlWithKey' etoc acc ebnfProds

  etoc :: Grammar -> NTermName -> [EBNF.ProductionRHS] -> Grammar
  etoc grammar nterm ebnf_rhss = let
    acc = (grammar, nterm, [])
    (grammar', _, rhss) = foldr etoc2 acc ebnf_rhss
    grammar'' = Map.insert nterm rhss grammar'
    in grammar''

  etoc2 :: EBNF.ProductionRHS -> 
           (Grammar, NTermName, [ProductionRHS]) -> 
           (Grammar, NTermName, [ProductionRHS])
  etoc2 ebnf_rhs (grammar, nterm, rhss)  = let
    acc = (grammar, nterm, [])
    (grammar', _, rhs) = foldr etoc3 acc ebnf_rhs 
    in (grammar', nterm, rhs:rhss) 
 
  etoc3 :: EBNF.Symbol -> 
           (Grammar, NTermName, ProductionRHS) -> 
           (Grammar, NTermName, ProductionRHS)
  etoc3 (EBNF.Term name) (grammar, nterm, rhs) = 
    (grammar, nterm, (Term name):rhs)

  etoc3 (EBNF.NTerm name) (grammar, nterm, rhs) =
    (grammar, nterm, (NTerm name):rhs)

  etoc3 (EBNF.Option syms) (grammar, nterm, rhs) = let
    tmpName = nterm ++ "Option"
    newNTermName = checkNTermName grammar tmpName
    newNTerm = NTerm newNTermName
    grammar' = Map.insert newNTermName [] grammar
    acc = (grammar', newNTermName, []) 
    (grammar'', _, newRHS) = foldr etoc3 acc syms 
    newRHSs = [[],newRHS]
    grammar''' = Map.insert newNTermName newRHSs grammar''
    in (grammar''', nterm, newNTerm:rhs) 

  etoc3 (EBNF.Many syms) (grammar, nterm, rhs) = let
    tmpName = nterm ++ "Many"
    newNTermName = checkNTermName grammar tmpName
    newNTerm = NTerm newNTermName
    grammar' = Map.insert newNTermName [] grammar
    acc = (grammar', newNTermName, []) 
    (grammar'', _, newRHS) = foldr etoc3 acc syms 
    newRHS' = newRHS ++ [newNTerm]
    newRHSs = [[],newRHS']
    grammar''' = Map.insert newNTermName newRHSs grammar''
    in (grammar''', nterm, newNTerm:rhs) 

-- Substitute head NonTerminal with its right hand side productions,
-- and return the new RHS productions.
subHeadNTerm :: Grammar -> ProductionRHS -> [ProductionRHS]
subHeadNTerm grammar ((NTerm name):rest) = let
  rhss = Maybe.fromJust $ Map.lookup name grammar
  in map (++rest) rhss
subHeadNTerm _ rhs = [rhs]

-- Substitute all head NonTerminals with their RHS Productions
subAllHeadNTerms :: Grammar -> Grammar
subAllHeadNTerms grammar = subAllHeadNTerms_ grammar (Map.keys grammar)

subAllHeadNTerms_ :: Grammar -> [NTermName] -> Grammar
subAllHeadNTerms_ grammar nterms = foldl' go grammar nterms where
  go :: Grammar -> NTermName -> Grammar
  go grammar nterm = let
    rhss = Maybe.fromJust $ Map.lookup nterm grammar
    rhss' = go' grammar rhss
    grammar' = Map.insert nterm rhss' grammar
    in grammar'
  
  go' :: Grammar -> [ProductionRHS] -> [ProductionRHS]
  go' grammar rhss = let
    split ((NTerm _):_) = True
    split _ = False
    (ntermRHSs, rest) = partition split rhss
    in if null ntermRHSs then rest
    else let
      subbedRHSs = concat (map (subHeadNTerm grammar) ntermRHSs)
      in rest ++ (go' grammar subbedRHSs)


-- Removes all common prefixes a NTerminal's RHS productions.
-- Returns new grammar with new NTerm names created.
fixAllCommonPrefix :: Grammar -> Grammar
fixAllCommonPrefix grammar = 
  fst (fixAllCommonPrefix_ grammar (Map.keys grammar))

fixAllCommonPrefix_ :: Grammar -> [NTermName] -> (Grammar, [NTermName])
fixAllCommonPrefix_ grammar nterms = foldl' go (grammar, []) nterms where
  -- Fold function over list of NTerm names. 
  -- Accumulates new Grammar and list of new NTerm names
  go :: (Grammar, [NTermName]) -> NTermName -> (Grammar, [NTermName])
  go (grammar, newNTerms) nterm = let
    addProds :: (Grammar, [NTermName], [ProductionRHS]) -> PrefixPair -> 
                (Grammar, [NTermName], [ProductionRHS])
    addProds (grammar, newNTerms, rhss) (prefix, hasPrefix) = let
      tmpName = nterm ++ "PTail"
      newNTermName = checkNTermName grammar tmpName
      prefixLen = length prefix
      newRHSs = map (drop prefixLen) hasPrefix
      grammar' = Map.insert newNTermName newRHSs grammar 
      prefix' = prefix ++ [(NTerm newNTermName)]
      rhss' = prefix':rhss
      in (grammar', newNTermName:newNTerms, rhss')

    rhss' = Maybe.fromJust $ Map.lookup nterm grammar
    (prefixPairs, rest) = commonPrefixes rhss'

    in if (null prefixPairs) then let
      grammar' = Map.insert nterm rhss' grammar
      in (grammar', newNTerms)
    else let
      acc = (grammar, [], rest)
      (grammar', newNTerms', rhss'') = foldl' addProds acc prefixPairs
      grammar'' = Map.insert nterm rhss'' grammar'
      (grammar''', newNTerms'') = 
        foldl' go (grammar'', newNTerms'++newNTerms) newNTerms'
      in (grammar''', newNTerms'')

-- Finds longest longest prefix to match most number of RHS productions.
-- Returns prefix and RHS productions it matched, along with the rest 
-- of unmatched rhs prods.
commonPrefix :: Prefix -> [ProductionRHS] -> (PrefixPair, [ProductionRHS])
commonPrefix prefix rhss = cp [] prefix [] [] rhss where
  cp :: Prefix -> Prefix -> [ProductionRHS] -> [ProductionRHS] -> 
        [ProductionRHS] -> (PrefixPair, [ProductionRHS])
  cp [] [] _ _ rhss = (([], []), rhss)

  cp lastPrefix [] lastHasPrefix lastRest _ = 
    ((lastPrefix, lastHasPrefix), lastRest)

  cp [] (p:prefixTail) _ _ rhss = let
    (hasPrefix, rest) = partition (isPrefixOf [p]) rhss
    in if (length hasPrefix) > 0 then
      cp [p] prefixTail hasPrefix rest rhss
    else (([],[]), rhss)

  cp lastPrefix (p:prefixTail) lastHasPrefix lastRest rhss = let
    prefix = lastPrefix ++ [p]
    (hasPrefix, rest) = partition (isPrefixOf prefix) rhss
    in if (length hasPrefix) == (length lastHasPrefix) then
      cp prefix prefixTail hasPrefix rest rhss
    else ((lastPrefix, lastHasPrefix), lastRest)
 
-- Find all common prefixes, and return as list of pairs of prefixes and
-- matched rhs prods, and a list of rest of rhs prods.
commonPrefixes :: [ProductionRHS] -> ([PrefixPair], [ProductionRHS])
commonPrefixes rhss = cp [] [] rhss where
  cp :: [PrefixPair] -> [ProductionRHS] -> [ProductionRHS] -> 
        ([PrefixPair], [ProductionRHS])

  cp lastPrefixPairs lastRest [] = (lastPrefixPairs, lastRest)

  cp lastPrefixPairs lastRest (rhs:rhss) = let
    ((prefix, hasPrefix), rest) = commonPrefix rhs rhss
    in if null prefix then 
      cp lastPrefixPairs (rhs:lastRest) rhss
    else let
      prefixPairs = (prefix,rhs:hasPrefix):lastPrefixPairs
      in cp prefixPairs lastRest rest

-- Remove all leading NTerminals in RHS productions, which have been 
-- checked for left recursion, and remove direct left recursion.
-- Return new grammar and list of new NTerm names created.
fixAllLeftRecur :: Grammar -> Grammar
fixAllLeftRecur grammar = fst (fixAllLeftRecur_ grammar (Map.keys grammar))

fixAllLeftRecur_ :: Grammar -> [NTermName] -> (Grammar, [NTermName])
fixAllLeftRecur_ grammar nterms = falr grammar nterms [] Set.empty where
  falr :: Grammar -> [NTermName] -> [NTermName] -> 
          NTermSet -> (Grammar, [NTermName])
  falr grammar [] newNTerms _ = (grammar, newNTerms)
  falr grammar (ntermName:ntermNames) newNTermNames doneSet = let
    subNTerms :: Grammar -> [ProductionRHS] -> [ProductionRHS]
    subNTerms grammar rhss = let
      split ((NTerm name):_) = Set.member name doneSet
      split _ = False
      (ntermRHSs, rest) = partition split rhss
      in if null ntermRHSs then rest
      else let
        subbedRHSs = concat (map (subHeadNTerm grammar) ntermRHSs)
        in rest ++ (subNTerms grammar subbedRHSs)

    rhss = Maybe.fromJust $ Map.lookup ntermName grammar
    rhss' = subNTerms grammar rhss
    doneSet' = Set.insert ntermName doneSet
    newNTermName = checkNTermName grammar (ntermName ++ "Tail")
    (rhss'', newRHSs) = leftRecur ntermName rhss' newNTermName
    grammar' = Map.insert ntermName rhss'' grammar
    in
      if null newRHSs then 
        falr grammar' ntermNames newNTermNames doneSet'
      else let
        grammar'' = Map.insert newNTermName newRHSs grammar'
        ntermNames' = newNTermName:ntermNames
        newNTermNames' = newNTermName:newNTermNames
        in falr grammar'' ntermNames' newNTermNames' doneSet'
 
-- Takes a nterm name and that nterm's RHS productions, as well as a new
-- nterm name in case there is left recurision. Removes direct 
-- left recursion to that nterm, and returns the modified RHS prods, and 
-- the new RHS prods for the new nterm. If there is no left recurion, then
-- the original RHS prods are returns along with an empty list.
leftRecur :: NTermName -> [ProductionRHS] -> NTermName ->
            ([ProductionRHS], [ProductionRHS])
leftRecur ntermName rhss newNTermName = let
  split ((NTerm name) : _) = ntermName == name
  split _ = False

  (lrecRHSs, rest) = partition split rhss
  in
    if null lrecRHSs then
      (rest, [])
    else let
      newNTerm = NTerm newNTermName
      newRHSs = [] : (map ((++ [newNTerm]) . tail) lrecRHSs)
      rhss' = map (++ [newNTerm]) rest
      in (rhss', newRHSs) 


