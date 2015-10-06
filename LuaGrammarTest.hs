import qualified Data.Map.Strict as Map
import qualified LLGrammar as LL
import EBNF

main :: IO ()
main = do
  let ebnfGrammar = Map.fromList ebnfGrammarList
  let grammar = LL.ebnfToCFG ebnfGrammar
  let grammarStr = LL.grammarToStr grammar
  writeFile "LuaGrammar_CFG.txt" $ 
    "Lua Grammar as CFG:\n" ++ grammarStr ++ "\n\n"

  let grammar' = LL.fixAllLeftRecur grammar
  let grammarStr' = LL.grammarToStr grammar' 
  writeFile "LuaGrammar_NoRec.txt" $ 
    "Lua Grammar with no left recursion:\n" ++ grammarStr' ++ "\n\n"

  let grammar'' = LL.fixAllCommonPrefix grammar'
  let grammarStr'' = LL.grammarToStr grammar''
  writeFile "LuaGrammar_NoRecCPrfx.txt" $ 
    "Lua Grammar with no common prefixes or left recurion:\n" ++ 
    grammarStr'' ++ "\n\n"

-- Lua 5.2 grammar from lua.org/manual/5.2/manual.html 
ebnfGrammarList :: [(String, [[Symbol]])]
ebnfGrammarList = [
  ("block", [ 
    [Many [(NTerm "stat")], Option [NTerm "retstat"]] 
    ]),
  ("stat", [
    [Term ";"],
    [NTerm "varlist", Term "=", NTerm "explist"],
    [NTerm "functioncall"],
    [NTerm "label"],
    [Term "break"],
    [Term "goto", Term "Name"],
    [Term "do", NTerm "block", Term "end"],
    [Term "while", NTerm "exp", Term "do", NTerm "block", Term "end"],
    [Term "repeat", NTerm "block", Term "until", NTerm "exp"],
    [Term "if", NTerm "exp", Term "then", NTerm "block", 
      Many [Term "elseif", NTerm "exp", Term "then", NTerm "block"],
      Option [Term "else", NTerm "block"], Term "end"],
    [Term "for", Term "Name", Term "=", NTerm "exp", Term ",", NTerm "exp",
      Option [Term ",", NTerm "exp"], Term "do", NTerm "block", Term "end"],
    [Term "for", NTerm "namelist", Term "in", NTerm "explist", Term "do",
      NTerm "block", Term "end"], 
    [Term "function", NTerm "funcname", NTerm "funcbody"],
    [Term "local", Term "function", Term "Name", NTerm "funcbody"],
    [Term "local", NTerm "namelist", Option [Term "=", NTerm "explist"]]
    ]),
  ("retstat", [ 
    [Term "return", Option [NTerm "explist"], 
      Option [Term ";"]] 
    ]),
  ("label", [ 
    [Term "::", Term "Name", Term "::"] 
    ]),
  ("funcname", [ 
    [Term "Name", Many [Term ".", Term "Name"], 
      Option [Term ":", Term "Name"]] 
    ]),
  ("varlist", [ 
    [NTerm "var", Many [Term ",", NTerm "var"]] ]),
  ("var", [ 
    [Term "Name"], 
    [NTerm "prefixexp", Term "[", NTerm "exp", Term "]"],
    [NTerm "prefixexp", Term ".", Term "Name"] 
    ]),
  ("namelist", [ 
    [Term "Name", Many [Term ",", Term "Name"]] 
    ]),
  ("explist", [ 
    [NTerm "exp", Many [Term ",", NTerm "exp"]] 
    ]),
  ("exp", [ 
    [Term "nil"], [Term "false"], [Term "true"], [Term "Number"], 
    [Term "String"], [Term "..."], [NTerm "functiondef"],
    [NTerm "prefixexp"], [NTerm "tableconstructor"], 
    [NTerm "exp", NTerm "binop", NTerm "exp"], [NTerm "unop", NTerm "exp"]
    ]),
  ("prefixexp", [ 
    [NTerm "var"], [NTerm "functioncall"], [Term "(", NTerm "exp", Term ")"] 
    ]),
  ("functioncall", [ 
    [NTerm "prefixexp", NTerm "args"],
    [NTerm "prefixexp", Term ":", Term "Name", NTerm "args"] 
    ]),
  ("args", [ 
    [Term "(", Option [NTerm "explist"], Term ")"],
    [NTerm "tableconstructor"], [Term "String"] 
    ]),
  ("functiondef", [ [Term "function", NTerm "funcbody"] ]),
  ("funcbody", [ 
    [Term "(", Option [NTerm "parlist"], Term ")", 
     NTerm "block", Term "end"] 
    ]),
  ("parlist", [ 
    [NTerm "namelist", Option [Term ",", Term "..."]], [Term "..."] 
    ]),
  ("tableconstructor", [
    [Term "{", Option [NTerm "fieldlist"], Term "}"]
    ]),
  ("fieldlist", [
    [NTerm "field", Many [NTerm "fieldsep", NTerm "field"], 
      Option [NTerm "fieldsep"]]
    ]),
  ("field", [
    [Term "[", NTerm "exp", Term "]", Term "=", NTerm "exp"],
    [Term "Name", Term "=", NTerm "exp"], [NTerm "exp"]
    ]),
  ("fieldsep", [
    [Term ","], [Term ";"]
    ]),
  ("binop", [
    [Term "+"], [Term "-"], [Term "*"], [Term "/"], [Term "^"], [Term "%"],
    [Term ".."], [Term "<"], [Term "<="], [Term ">"], [Term ">="], 
    [Term "=="], [Term "~="], [Term "and"], [Term "or"]
    ]),
  ("unop", [
    [Term "-"], [Term "not"], [Term "#"]
    ])
  ]
  
    

