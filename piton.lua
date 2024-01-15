--
-- This is file `piton.lua',
-- generated with the docstrip utility.
--
-- The original source files were:
--
-- piton.dtx  (with options: `LUA')
-- ---------------------------------------------
-- Copyright (C) 2022-2024 by F. Pantigny
-- 
-- This file may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License, either
-- version 1.3 of this license or (at your option) any later
-- version.  The latest version of this license is in:
-- 
--      http://www.latex-project.org/lppl.txt
-- 
-- and version 1.3 or later is part of all distributions of
-- LaTeX version 2005/12/01 or later.
-- -------------------------------------------
-- 
-- This file is part of the LuaLaTeX package 'piton'.
-- Version 2.4 of 2024/01/15


if piton.comment_latex == nil then piton.comment_latex = ">" end
piton.comment_latex = "#" .. piton.comment_latex
function piton.open_brace ()
   tex.sprint("{")
end
function piton.close_brace ()
   tex.sprint("}")
end
local P, S, V, C, Ct, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.Ct, lpeg.Cc
local Cf, Cs , Cg , Cmt , Cb = lpeg.Cf, lpeg.Cs, lpeg.Cg , lpeg.Cmt , lpeg.Cb
local R = lpeg.R
local function Q(pattern)
  return Ct ( Cc ( luatexbase.catcodetables.CatcodeTableOther ) * C ( pattern ) )
end
local function L(pattern)
  return Ct ( C ( pattern ) )
end
local function Lc(string)
  return Cc ( { luatexbase.catcodetables.expl , string } )
end
local function K(style, pattern)
  return
     Lc ( "{\\PitonStyle{" .. style .. "}{" )
     * Q ( pattern )
     * Lc ( "}}" )
end
local function WithStyle(style,pattern)
  return
       Ct ( Cc "Open" * Cc ( "{\\PitonStyle{" .. style .. "}{" ) * Cc "}}" )
     * pattern
     * Ct ( Cc "Close" )
end
Escape = P ( false )
if piton.begin_escape ~= nil
then
  Escape =
    P(piton.begin_escape)
    * L ( ( 1 - P(piton.end_escape) ) ^ 1 )
    * P(piton.end_escape)
end
EscapeMath = P ( false )
if piton.begin_escape_math ~= nil
then
  EscapeMath =
    P(piton.begin_escape_math)
    * Lc ( "\\ensuremath{" )
    * L ( ( 1 - P(piton.end_escape_math) ) ^ 1 )
    * Lc ( "}" )
    * P(piton.end_escape_math)
end
lpeg.locale(lpeg)
local alpha, digit = lpeg.alpha, lpeg.digit
local space = P " "
local letter = alpha + P "_"
  + P "â" + P "à" + P "ç" + P "é" + P "è" + P "ê" + P "ë" + P "ï" + P "î"
  + P "ô" + P "û" + P "ü" + P "Â" + P "À" + P "Ç" + P "É" + P "È" + P "Ê"
  + P "Ë" + P "Ï" + P "Î" + P "Ô" + P "Û" + P "Ü"

local alphanum = letter + digit
local identifier = letter * alphanum ^ 0
local Identifier = K ( 'Identifier' , identifier )
local Number =
  K ( 'Number' ,
      ( digit^1 * P "." * digit^0 + digit^0 * P "." * digit^1 + digit^1 )
      * ( S "eE" * S "+-" ^ -1 * digit^1 ) ^ -1
      + digit^1
    )
local Word
if piton.begin_escape ~= nil -- before : ''
then Word = Q ( ( ( 1 - space - P(piton.begin_escape) - P(piton.end_escape) )
                   - S "'\"\r[()]" - digit ) ^ 1 )
else Word = Q ( ( ( 1 - space ) - S "'\"\r[()]" - digit ) ^ 1 )
end
local Space = ( Q " " ) ^ 1

local SkipSpace = ( Q " " ) ^ 0

local Punct = Q ( S ".,:;!" )

local Tab = P "\t" * Lc ( '\\l__piton_tab_tl' )
local SpaceIndentation = Lc ( '\\__piton_an_indentation_space:' ) * ( Q " " )
local Delim = Q ( S "[()]" )
local VisualSpace = space * Lc "\\l__piton_space_tl"
local Beamer = P ( false )
local BeamerBeginEnvironments = P ( true )
local BeamerEndEnvironments = P ( true )
if piton_beamer
then
  local BeamerNamesEnvironments =
    P "uncoverenv" + P "onlyenv" + P "visibleenv" + P "invisibleenv"
    + P "alertenv" + P "actionenv"
  BeamerBeginEnvironments =
      ( space ^ 0 *
        L
          (
            P "\\begin{" * BeamerNamesEnvironments * "}"
            * ( P "<" * ( 1 - P ">" ) ^ 0 * P ">" ) ^ -1
          )
        * P "\r"
      ) ^ 0
  BeamerEndEnvironments =
      ( space ^ 0 *
        L ( P "\\end{" * BeamerNamesEnvironments * P "}" )
        * P "\r"
      ) ^ 0
  function OneBeamerEnvironment(name,lpeg)
    return
        Ct ( Cc "Open"
              * C (
                    P ( "\\begin{" .. name ..   "}" )
                    * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  )
             * Cc ( "\\end{" .. name ..  "}" )
            )
       * (
           C ( ( 1 - P ( "\\end{" .. name .. "}" ) ) ^ 0 )
           / ( function (s) return lpeg : match(s) end )
         )
       * P ( "\\end{" .. name ..  "}" ) * Ct ( Cc "Close" )
  end
end
local languages = { }
local Operator =
  K ( 'Operator' ,
      P "!=" + P "<>" + P "==" + P "<<" + P ">>" + P "<=" + P ">=" + P ":="
      + P "//" + P "**" + S "-~+/*%=<>&.@|"
    )

local OperatorWord =
  K ( 'Operator.Word' , P "in" + P "is" + P "and" + P "or" + P "not" )

local Keyword =
  K ( 'Keyword' ,
      P "as" + P "assert" + P "break" + P "case" + P "class" + P "continue"
      + P "def" + P "del" + P "elif" + P "else" + P "except" + P "exec"
      + P "finally" + P "for" + P "from" + P "global" + P "if" + P "import"
      + P "lambda" + P "non local" + P "pass" + P "return" + P "try"
      + P "while" + P "with" + P "yield" + P "yield from" )
  + K ( 'Keyword.Constant' ,P "True" + P "False" + P "None" )

local Builtin =
  K ( 'Name.Builtin' ,
      P "__import__" + P "abs" + P "all" + P "any" + P "bin" + P "bool"
    + P "bytearray" + P "bytes" + P "chr" + P "classmethod" + P "compile"
    + P "complex" + P "delattr" + P "dict" + P "dir" + P "divmod"
    + P "enumerate" + P "eval" + P "filter" + P "float" + P "format"
    + P "frozenset" + P "getattr" + P "globals" + P "hasattr" + P "hash"
    + P "hex" + P "id" + P "input" + P "int" + P "isinstance" + P "issubclass"
    + P "iter" + P "len" + P "list" + P "locals" + P "map" + P "max"
    + P "memoryview" + P "min" + P "next" + P "object" + P "oct" + P "open"
    + P "ord" + P "pow" + P "print" + P "property" + P "range" + P "repr"
    + P "reversed" + P "round" + P "set" + P "setattr" + P "slice" + P "sorted"
    + P "staticmethod" + P "str" + P "sum" + P "super" + P "tuple" + P "type"
    + P "vars" + P "zip" )

local Exception =
  K ( 'Exception' ,
      P "ArithmeticError" + P "AssertionError" + P "AttributeError"
   + P "BaseException" + P "BufferError" + P "BytesWarning" + P "DeprecationWarning"
   + P "EOFError" + P "EnvironmentError" + P "Exception" + P "FloatingPointError"
   + P "FutureWarning" + P "GeneratorExit" + P "IOError" + P "ImportError"
   + P "ImportWarning" + P "IndentationError" + P "IndexError" + P "KeyError"
   + P "KeyboardInterrupt" + P "LookupError" + P "MemoryError" + P "NameError"
   + P "NotImplementedError" + P "OSError" + P "OverflowError"
   + P "PendingDeprecationWarning" + P "ReferenceError" + P "ResourceWarning"
   + P "RuntimeError" + P "RuntimeWarning" + P "StopIteration"
   + P "SyntaxError" + P "SyntaxWarning" + P "SystemError" + P "SystemExit"
   + P "TabError" + P "TypeError" + P "UnboundLocalError" + P "UnicodeDecodeError"
   + P "UnicodeEncodeError" + P "UnicodeError" + P "UnicodeTranslateError"
   + P "UnicodeWarning" + P "UserWarning" + P "ValueError" + P "VMSError"
   + P "Warning" + P "WindowsError" + P "ZeroDivisionError"
   + P "BlockingIOError" + P "ChildProcessError" + P "ConnectionError"
   + P "BrokenPipeError" + P "ConnectionAbortedError" + P "ConnectionRefusedError"
   + P "ConnectionResetError" + P "FileExistsError" + P "FileNotFoundError"
   + P "InterruptedError" + P "IsADirectoryError" + P "NotADirectoryError"
   + P "PermissionError" + P "ProcessLookupError" + P "TimeoutError"
   + P "StopAsyncIteration" + P "ModuleNotFoundError" + P "RecursionError" )

local RaiseException = K ( 'Keyword' , P "raise" ) * SkipSpace * Exception * Q ( P "(" )

local Decorator = K ( 'Name.Decorator' , P "@" * letter^1  )
local DefClass =
  K ( 'Keyword' , P "class" ) * Space * K ( 'Name.Class' , identifier )
local ImportAs =
  K ( 'Keyword' , P "import" )
   * Space
   * K ( 'Name.Namespace' ,
         identifier * ( P "." * identifier ) ^ 0 )
   * (
       ( Space * K ( 'Keyword' , P "as" ) * Space
          * K ( 'Name.Namespace' , identifier ) )
       +
       ( SkipSpace * Q ( P "," ) * SkipSpace
          * K ( 'Name.Namespace' , identifier ) ) ^ 0
     )
local FromImport =
  K ( 'Keyword' , P "from" )
    * Space * K ( 'Name.Namespace' , identifier )
    * Space * K ( 'Keyword' , P "import" )
local PercentInterpol =
  K ( 'String.Interpol' ,
      P "%"
      * ( P "(" * alphanum ^ 1 * P ")" ) ^ -1
      * ( S "-#0 +" ) ^ 0
      * ( digit ^ 1 + P "*" ) ^ -1
      * ( P "." * ( digit ^ 1 + P "*" ) ) ^ -1
      * ( S "HlL" ) ^ -1
      * S "sdfFeExXorgiGauc%"
    )
local SingleShortString =
  WithStyle ( 'String.Short' ,
         Q ( P "f'" + P "F'" )
         * (
             K ( 'String.Interpol' , P "{" )
              * K ( 'Interpol.Inside' , ( 1 - S "}':" ) ^ 0  )
              * Q ( P ":" * (1 - S "}:'") ^ 0 ) ^ -1
              * K ( 'String.Interpol' , P "}" )
             +
             VisualSpace
             +
             Q ( ( P "\\'" + P "{{" + P "}}" + 1 - S " {}'" ) ^ 1 )
           ) ^ 0
         * Q ( P "'" )
       +
         Q ( P "'" + P "r'" + P "R'" )
         * ( Q ( ( P "\\'" + 1 - S " '\r%" ) ^ 1 )
             + VisualSpace
             + PercentInterpol
             + Q ( P "%" )
           ) ^ 0
         * Q ( P "'" ) )

local DoubleShortString =
  WithStyle ( 'String.Short' ,
         Q ( P "f\"" + P "F\"" )
         * (
             K ( 'String.Interpol' , P "{" )
               * Q ( ( 1 - S "}\":" ) ^ 0 , 'Interpol.Inside' )
               * ( K ( 'String.Interpol' , P ":" ) * Q ( (1 - S "}:\"") ^ 0 ) ) ^ -1
               * K ( 'String.Interpol' , P "}" )
             +
             VisualSpace
             +
             Q ( ( P "\\\"" + P "{{" + P "}}" + 1 - S " {}\"" ) ^ 1 )
            ) ^ 0
         * Q ( P "\"" )
       +
         Q ( P "\"" + P "r\"" + P "R\"" )
         * ( Q ( ( P "\\\"" + 1 - S " \"\r%" ) ^ 1 )
             + VisualSpace
             + PercentInterpol
             + Q ( P "%" )
           ) ^ 0
         * Q ( P "\"" ) )

local ShortString = SingleShortString + DoubleShortString
local balanced_braces =
  P { "E" ,
       E =
           (
             P "{" * V "E" * P "}"
             +
             ShortString
             +
             ( 1 - S "{}" )
           ) ^ 0
    }
if piton_beamer
then
  Beamer =
      L ( P "\\pause" * ( P "[" * ( 1 - P "]" ) ^ 0 * P "]" ) ^ -1 )
    +
      Ct ( Cc "Open"
            * C (
                  (
                    P "\\uncover" + P "\\only" + P "\\alert" + P "\\visible"
                    + P "\\invisible" + P "\\action"
                  )
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopPython:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
    + OneBeamerEnvironment ( "uncoverenv" , MainLoopPython )
    + OneBeamerEnvironment ( "onlyenv" , MainLoopPython )
    + OneBeamerEnvironment ( "visibleenv" , MainLoopPython )
    + OneBeamerEnvironment ( "invisibleenv" , MainLoopPython )
    + OneBeamerEnvironment ( "alertenv" , MainLoopPython )
    + OneBeamerEnvironment ( "actionenv" , MainLoopPython )
    +
      L (
          ( P "\\alt" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
    +
      L (
          ( P "\\temporal" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
end
DetectedCommands =
      Ct ( Cc "Open"
            * C (
                  piton.ListCommands
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopPython:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
local PromptHastyDetection = ( # ( P ">>>" + P "..." ) * Lc ( '\\__piton_prompt:' ) ) ^ -1
local Prompt = K ( 'Prompt' , ( ( P ">>>" + P "..." ) * P " " ^ -1 ) ^ -1  )
local EOL =
  P "\r"
  *
  (
    ( space^0 * -1 )
    +
    Ct (
         Cc "EOL"
         *
         Ct (
              Lc "\\__piton_end_line:"
              * BeamerEndEnvironments
              * BeamerBeginEnvironments
              * PromptHastyDetection
              * Lc "\\__piton_newline: \\__piton_begin_line:"
              * Prompt
            )
       )
  )
  *
  SpaceIndentation ^ 0
local SingleLongString =
  WithStyle ( 'String.Long' ,
     ( Q ( S "fF" * P "'''" )
         * (
             K ( 'String.Interpol' , P "{"  )
               * K ( 'Interpol.Inside' , ( 1 - S "}:\r" - P "'''" ) ^ 0  )
               * Q ( P ":" * (1 - S "}:\r" - P "'''" ) ^ 0 ) ^ -1
               * K ( 'String.Interpol' , P "}"  )
             +
             Q ( ( 1 - P "'''" - S "{}'\r" ) ^ 1 )
             +
             EOL
           ) ^ 0
       +
         Q ( ( S "rR" ) ^ -1  * P "'''" )
         * (
             Q ( ( 1 - P "'''" - S "\r%" ) ^ 1 )
             +
             PercentInterpol
             +
             P "%"
             +
             EOL
           ) ^ 0
      )
      * Q ( P "'''" ) )

local DoubleLongString =
  WithStyle ( 'String.Long' ,
     (
        Q ( S "fF" * P "\"\"\"" )
        * (
            K ( 'String.Interpol', P "{"  )
              * K ( 'Interpol.Inside' , ( 1 - S "}:\r" - P "\"\"\"" ) ^ 0 )
              * Q ( P ":" * (1 - S "}:\r" - P "\"\"\"" ) ^ 0 ) ^ -1
              * K ( 'String.Interpol' , P "}"  )
            +
            Q ( ( 1 - P "\"\"\"" - S "{}\"\r" ) ^ 1 )
            +
            EOL
          ) ^ 0
      +
        Q ( ( S "rR" ) ^ -1  * P "\"\"\"" )
        * (
            Q ( ( 1 - P "\"\"\"" - S "%\r" ) ^ 1 )
            +
            PercentInterpol
            +
            P "%"
            +
            EOL
          ) ^ 0
     )
     * Q ( P "\"\"\"" )
  )
local LongString = SingleLongString + DoubleLongString
local StringDoc =
    K ( 'String.Doc' , P "\"\"\"" )
      * ( K ( 'String.Doc' , (1 - P "\"\"\"" - P "\r" ) ^ 0  ) * EOL
          * Tab ^ 0
        ) ^ 0
      * K ( 'String.Doc' , ( 1 - P "\"\"\"" - P "\r" ) ^ 0 * P "\"\"\"" )
local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$"

local Comment =
  WithStyle ( 'Comment' ,
     Q ( P "#" )
     * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 )
  * ( EOL + -1 )
local CommentLaTeX =
  P(piton.comment_latex)
  * Lc "{\\PitonStyle{Comment.LaTeX}{\\ignorespaces"
  * L ( ( 1 - P "\r" ) ^ 0 )
  * Lc "}}"
  * ( EOL + -1 )
local expression =
  P { "E" ,
       E = ( P "'" * ( P "\\'" + 1 - S "'\r" ) ^ 0 * P "'"
             + P "\"" * (P "\\\"" + 1 - S "\"\r" ) ^ 0 * P "\""
             + P "{" * V "F" * P "}"
             + P "(" * V "F" * P ")"
             + P "[" * V "F" * P "]"
             + ( 1 - S "{}()[]\r," ) ) ^ 0 ,
       F = ( P "{" * V "F" * P "}"
             + P "(" * V "F" * P ")"
             + P "[" * V "F" * P "]"
             + ( 1 - S "{}()[]\r\"'" ) ) ^ 0
    }
local Param =
  SkipSpace * Identifier * SkipSpace
   * (
         K ( 'InitialValues' , P "=" * expression )
       + Q ( P ":" ) * SkipSpace * K ( 'Name.Type' , letter ^ 1  )
     ) ^ -1
local Params = ( Param * ( Q "," * Param ) ^ 0 ) ^ -1
local DefFunction =
  K ( 'Keyword' , P "def" )
  * Space
  * K ( 'Name.Function.Internal' , identifier )
  * SkipSpace
  * Q ( P "(" ) * Params * Q ( P ")" )
  * SkipSpace
  * ( Q ( P "->" ) * SkipSpace * K ( 'Name.Type' , identifier  ) ) ^ -1
  * K ( 'ParseAgain' , ( 1 - S ":\r" )^0  )
  * Q ( P ":" )
  * ( SkipSpace
      * ( EOL + CommentLaTeX + Comment ) -- in all cases, that contains an EOL
      * Tab ^ 0
      * SkipSpace
      * StringDoc ^ 0 -- there may be additionnal docstrings
    ) ^ -1
local ExceptionInConsole = Exception *  Q ( ( 1 - P "\r" ) ^ 0 ) * EOL
local MainPython =
       EOL
     + Space
     + Tab
     + Escape + EscapeMath
     + CommentLaTeX
     + Beamer
     + DetectedCommands
     + LongString
     + Comment
     + ExceptionInConsole
     + Delim
     + Operator
     + OperatorWord * ( Space + Punct + Delim + EOL + -1 )
     + ShortString
     + Punct
     + FromImport
     + RaiseException
     + DefFunction
     + DefClass
     + Keyword * ( Space + Punct + Delim + EOL + -1 )
     + Decorator
     + Builtin * ( Space + Punct + Delim + EOL + -1 )
     + Identifier
     + Number
     + Word
MainLoopPython =
  (  ( space^1 * -1 )
     + MainPython
  ) ^ 0
local python = P ( true )

python =
  Ct (
       ( ( space - P "\r" ) ^0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * PromptHastyDetection
       * Lc '\\__piton_begin_line:'
       * Prompt
       * SpaceIndentation ^ 0
       * MainLoopPython
       * -1
       * Lc '\\__piton_end_line:'
     )
languages['python'] = python
local Delim = Q ( P "[|" + P "|]" + S "[()]" )
local Punct = Q ( S ",:;!" )
local cap_identifier = R "AZ" * ( R "az" + R "AZ" + S "_'" + digit ) ^ 0
local Constructor = K ( 'Name.Constructor' , cap_identifier )
local ModuleType = K ( 'Name.Type' , cap_identifier )
local identifier =
  ( R "az" + P "_") * ( R "az" + R "AZ" + S "_'" + digit ) ^ 0
local Identifier = K ( 'Identifier' , identifier )
local expression_for_fields =
  P { "E" ,
       E = ( P "{" * V "F" * P "}"
             + P "(" * V "F" * P ")"
             + P "[" * V "F" * P "]"
             + P "\"" * (P "\\\"" + 1 - S "\"\r" )^0 * P "\""
             + P "'" * ( P "\\'" + 1 - S "'\r" )^0 * P "'"
             + ( 1 - S "{}()[]\r;" ) ) ^ 0 ,
       F = ( P "{" * V "F" * P "}"
             + P "(" * V "F" * P ")"
             + P "[" * V "F" * P "]"
             + ( 1 - S "{}()[]\r\"'" ) ) ^ 0
    }
local OneFieldDefinition =
    ( K ( 'KeyWord' , P "mutable" ) * SkipSpace ) ^ -1
  * K ( 'Name.Field' , identifier ) * SkipSpace
  * Q ":" * SkipSpace
  * K ( 'Name.Type' , expression_for_fields )
  * SkipSpace

local OneField =
    K ( 'Name.Field' , identifier ) * SkipSpace
  * Q "=" * SkipSpace
  * ( C ( expression_for_fields ) / ( function (s) return LoopOCaml:match(s) end ) )
  * SkipSpace

local Record =
  Q "{" * SkipSpace
  *
    (
      OneFieldDefinition * ( Q ";" * SkipSpace * OneFieldDefinition ) ^ 0
      +
      OneField * ( Q ";" * SkipSpace * OneField ) ^ 0
    )
  *
  Q "}"
local DotNotation =
  (
      K ( 'Name.Module' , cap_identifier )
        * Q "."
        * ( Identifier + Constructor + Q "(" + Q "[" + Q "{" )

      +
      Identifier
        * Q "."
        * K ( 'Name.Field' , identifier )
  )
  * ( Q "." * K ( 'Name.Field' , identifier ) ) ^ 0
local Operator =
  K ( 'Operator' ,
      P "!=" + P "<>" + P "==" + P "<<" + P ">>" + P "<=" + P ">=" + P ":="
      + P "||" + P "&&" + P "//" + P "**" + P ";;" + P "::" + P "->"
      + P "+." + P "-." + P "*." + P "/."
      + S "-~+/*%=<>&@|"
    )

local OperatorWord =
  K ( 'Operator.Word' ,
      P "and" + P "asr" + P "land" + P "lor" + P "lsl" + P "lxor"
      + P "mod" + P "or" )

local Keyword =
  K ( 'Keyword' ,
      P "assert" + P "as" + P "begin" + P "class" + P "constraint" + P "done"
  + P "downto" + P "do" + P "else" + P "end" + P "exception" + P "external"
  + P "for" + P "function" + P "functor" + P "fun"  + P "if"
  + P "include" + P "inherit" + P "initializer" + P "in"  + P "lazy" + P "let"
  + P "match" + P "method" + P "module" + P "mutable" + P "new" + P "object"
  + P "of" + P "open" + P "private" + P "raise" + P "rec" + P "sig"
  + P "struct" + P "then" + P "to" + P "try" + P "type"
  + P "value" + P "val" + P "virtual" + P "when" + P "while" + P "with" )
  + K ( 'Keyword.Constant' , P "true" + P "false" )

local Builtin =
  K ( 'Name.Builtin' , P "not" + P "incr" + P "decr" + P "fst" + P "snd" )
local Exception =
  K (   'Exception' ,
       P "Division_by_zero" + P "End_of_File" + P "Failure"
     + P "Invalid_argument" + P "Match_failure" + P "Not_found"
     + P "Out_of_memory" + P "Stack_overflow" + P "Sys_blocked_io"
     + P "Sys_error" + P "Undefined_recursive_module" )
local Char =
  K ( 'String.Short' , P "'" * ( ( 1 - P "'" ) ^ 0 + P "\\'" ) * P "'" )
local balanced_braces =
  P { "E" ,
       E =
           (
             P "{" * V "E" * P "}"
             +
             P "\"" * ( 1 - S "\"" ) ^ 0 * P "\""  -- OCaml strings
             +
             ( 1 - S "{}" )
           ) ^ 0
    }
if piton_beamer
then
  Beamer =
      L ( P "\\pause" * ( P "[" * ( 1 - P "]" ) ^ 0 * P "]" ) ^ -1 )
    +
      Ct ( Cc "Open"
            * C (
                  (
                    P "\\uncover" + P "\\only" + P "\\alert" + P "\\visible"
                    + P "\\invisible" + P "\\action"
                  )
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopOCaml:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
    + OneBeamerEnvironment ( "uncoverenv" , MainLoopOCaml )
    + OneBeamerEnvironment ( "onlyenv" , MainLoopOCaml )
    + OneBeamerEnvironment ( "visibleenv" , MainLoopOCaml )
    + OneBeamerEnvironment ( "invisibleenv" , MainLoopOCaml )
    + OneBeamerEnvironment ( "alertenv" , MainLoopOCaml )
    + OneBeamerEnvironment ( "actionenv" , MainLoopOCaml )
    +
      L (
          ( P "\\alt" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
    +
      L (
          ( P "\\temporal" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
end
DetectedCommands =
      Ct ( Cc "Open"
            * C (
                  piton.ListCommands
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopOCaml:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
local EOL =
  P "\r"
  *
  (
    ( space^0 * -1 )
    +
    Ct (
         Cc "EOL"
         *
         Ct (
              Lc "\\__piton_end_line:"
              * BeamerEndEnvironments
              * BeamerBeginEnvironments
              * PromptHastyDetection
              * Lc "\\__piton_newline: \\__piton_begin_line:"
              * Prompt
            )
       )
  )
  *
  SpaceIndentation ^ 0
local ocaml_string =
       Q ( P "\"" )
     * (
         VisualSpace
         +
         Q ( ( 1 - S " \"\r" ) ^ 1 )
         +
         EOL
       ) ^ 0
     * Q ( P "\"" )
local String = WithStyle ( 'String.Long' , ocaml_string )
local ext = ( R "az" + P "_" ) ^ 0
local open = "{" * Cg(ext, 'init') * "|"
local close = "|" * C(ext) * "}"
local closeeq =
  Cmt ( close * Cb('init'),
        function (s, i, a, b) return a==b end )
local QuotedStringBis =
  WithStyle ( 'String.Long' ,
      (
        Space
        +
        Q ( ( 1 - S " \r" ) ^ 1 )
        +
        EOL
      ) ^ 0  )

local QuotedString =
   C ( open * ( 1 - closeeq ) ^ 0  * close ) /
  ( function (s) return QuotedStringBis : match(s) end )
local Comment =
  WithStyle ( 'Comment' ,
     P {
         "A" ,
         A = Q "(*"
             * ( V "A"
                 + Q ( ( 1 - P "(*" - P "*)" - S "\r$\"" ) ^ 1 ) -- $
                 + ocaml_string
                 + P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1 ) * P "$" -- $
                 + EOL
               ) ^ 0
             * Q "*)"
       }   )
local balanced_parens =
  P { "E" ,
       E =
           (
             P "(" * V "E" * P ")"
             +
             ( 1 - S "()" )
           ) ^ 0
    }
local Argument =
  K ( 'Identifier' , identifier )
  + Q "(" * SkipSpace
    * K ( 'Identifier' , identifier ) * SkipSpace
    * Q ":" * SkipSpace
    * K ( 'Name.Type' , balanced_parens ) * SkipSpace
    * Q ")"
local DefFunction =
  K ( 'Keyword' , P "let open" )
   * Space
   * K ( 'Name.Module' , cap_identifier )
  +
  K ( 'Keyword' , P "let rec" + P "let" + P "and" )
    * Space
    * K ( 'Name.Function.Internal' , identifier )
    * Space
    * (
        Q "=" * SkipSpace * K ( 'Keyword' , P "function" )
        +
        Argument
         * ( SkipSpace * Argument ) ^ 0
         * (
             SkipSpace
             * Q ":"
             * K ( 'Name.Type' , ( 1 - P "=" ) ^ 0 )
           ) ^ -1
      )
local DefModule =
  K ( 'Keyword' , P "module" ) * Space
  *
    (
          K ( 'Keyword' , P "type" ) * Space
        * K ( 'Name.Type' , cap_identifier )
      +
        K ( 'Name.Module' , cap_identifier ) * SkipSpace
        *
          (
            Q "(" * SkipSpace
              * K ( 'Name.Module' , cap_identifier ) * SkipSpace
              * Q ":" * SkipSpace
              * K ( 'Name.Type' , cap_identifier ) * SkipSpace
              *
                (
                  Q "," * SkipSpace
                    * K ( 'Name.Module' , cap_identifier ) * SkipSpace
                    * Q ":" * SkipSpace
                    * K ( 'Name.Type' , cap_identifier ) * SkipSpace
                ) ^ 0
              * Q ")"
          ) ^ -1
        *
          (
            Q "=" * SkipSpace
            * K ( 'Name.Module' , cap_identifier )  * SkipSpace
            * Q "("
            * K ( 'Name.Module' , cap_identifier ) * SkipSpace
              *
              (
                Q ","
                *
                K ( 'Name.Module' , cap_identifier ) * SkipSpace
              ) ^ 0
            * Q ")"
          ) ^ -1
    )
  +
  K ( 'Keyword' , P "include" + P "open" )
  * Space * K ( 'Name.Module' , cap_identifier )
local TypeParameter = K ( 'TypeParameter' , P "'" * alpha * # ( 1 - P "'" ) )
MainOCaml =
       EOL
     + Space
     + Tab
     + Escape + EscapeMath
     + Beamer
     + DetectedCommands
     + TypeParameter
     + String + QuotedString + Char
     + Comment
     + Delim
     + Operator
     + Punct
     + FromImport
     + Exception
     + DefFunction
     + DefModule
     + Record
     + Keyword * ( Space + Punct + Delim + EOL + -1 )
     + OperatorWord * ( Space + Punct + Delim + EOL + -1 )
     + Builtin * ( Space + Punct + Delim + EOL + -1 )
     + DotNotation
     + Constructor
     + Identifier
     + Number
     + Word

LoopOCaml = MainOCaml ^ 0

MainLoopOCaml =
  (  ( space^1 * -1 )
     + MainOCaml
  ) ^ 0
local ocaml = P ( true )

ocaml =
  Ct (
       ( ( space - P "\r" ) ^0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc ( '\\__piton_begin_line:' )
       * SpaceIndentation ^ 0
       * MainLoopOCaml
       * -1
       * Lc ( '\\__piton_end_line:' )
     )
languages['ocaml'] = ocaml
local Delim = Q ( S "{[()]}" )
local Punct = Q ( S ",:;!" )
local identifier = letter * alphanum ^ 0

local Operator =
  K ( 'Operator' ,
      P "!=" + P "==" + P "<<" + P ">>" + P "<=" + P ">="
      + P "||" + P "&&" + S "-~+/*%=<>&.@|!"
    )

local Keyword =
  K ( 'Keyword' ,
      P "alignas" + P "asm" + P "auto" + P "break" + P "case" + P "catch"
      + P "class" + P "const" + P "constexpr" + P "continue"
      + P "decltype" + P "do" + P "else" + P "enum" + P "extern"
      + P "for" + P "goto" + P "if" + P "nexcept" + P "private" + P "public"
      + P "register" + P "restricted" + P "return" + P "static" + P "static_assert"
      + P "struct" + P "switch" + P "thread_local" + P "throw" + P "try"
      + P "typedef" + P "union" + P "using" + P "virtual" + P "volatile"
      + P "while"
    )
  + K ( 'Keyword.Constant' ,
        P "default" + P "false" + P "NULL" + P "nullptr" + P "true"
      )

local Builtin =
  K ( 'Name.Builtin' ,
      P "alignof" + P "malloc" + P "printf" + P "scanf" + P "sizeof"
    )

local Type =
  K ( 'Name.Type' ,
      P "bool" + P "char" + P "char16_t" + P "char32_t" + P "double"
      + P "float" + P "int" + P "int8_t" + P "int16_t" + P "int32_t"
      + P "int64_t" + P "long" + P "short" + P "signed" + P "unsigned"
      + P "void" + P "wchar_t"
    )

local DefFunction =
  Type
  * Space
  * Q ( "*" ) ^ -1
  * K ( 'Name.Function.Internal' , identifier )
  * SkipSpace
  * # P "("
local DefClass =
  K ( 'Keyword' , P "class" ) * Space * K ( 'Name.Class' , identifier )
local String =
  WithStyle ( 'String.Long' ,
      Q "\""
      * ( VisualSpace
          + K ( 'String.Interpol' ,
                P "%" * ( S "difcspxXou" + P "ld" + P "li" + P "hd" + P "hi" )
              )
          + Q ( ( P "\\\"" + 1 - S " \"" ) ^ 1 )
        ) ^ 0
      * Q "\""
    )
local balanced_braces =
  P { "E" ,
       E =
           (
             P "{" * V "E" * P "}"
             +
             String
             +
             ( 1 - S "{}" )
           ) ^ 0
    }
if piton_beamer
then
  Beamer =
      L ( P "\\pause" * ( P "[" * ( 1 - P "]" ) ^ 0 * P "]" ) ^ -1 )
    +
      Ct ( Cc "Open"
            * C (
                  (
                    P "\\uncover" + P "\\only" + P "\\alert" + P "\\visible"
                    + P "\\invisible" + P "\\action"
                  )
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopC:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
    + OneBeamerEnvironment ( "uncoverenv" , MainLoopC )
    + OneBeamerEnvironment ( "onlyenv" , MainLoopC )
    + OneBeamerEnvironment ( "visibleenv" , MainLoopC )
    + OneBeamerEnvironment ( "invisibleenv" , MainLoopC )
    + OneBeamerEnvironment ( "alertenv" , MainLoopC )
    + OneBeamerEnvironment ( "actionenv" , MainLoopC )
    +
      L (
          ( P "\\alt" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
    +
      L (
          ( P "\\temporal" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
end
DetectedCommands =
      Ct ( Cc "Open"
            * C (
                  piton.ListCommands
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopC:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
local EOL =
  P "\r"
  *
  (
    ( space^0 * -1 )
    +
    Ct (
         Cc "EOL"
         *
         Ct (
              Lc "\\__piton_end_line:"
              * BeamerEndEnvironments
              * BeamerBeginEnvironments
              * PromptHastyDetection
              * Lc "\\__piton_newline: \\__piton_begin_line:"
              * Prompt
            )
       )
  )
  *
  SpaceIndentation ^ 0
local Preproc =
  K ( 'Preproc' , P "#" * (1 - P "\r" ) ^ 0  ) * ( EOL + -1 )
local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$"

local Comment =
  WithStyle ( 'Comment' ,
     Q ( P "//" )
     * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 )
  * ( EOL + -1 )

local LongComment =
  WithStyle ( 'Comment' ,
               Q ( P "/*" )
               * ( CommentMath + Q ( ( 1 - P "*/" - S "$\r" ) ^ 1 ) + EOL ) ^ 0
               * Q ( P "*/" )
            ) -- $
local CommentLaTeX =
  P(piton.comment_latex)
  * Lc "{\\PitonStyle{Comment.LaTeX}{\\ignorespaces"
  * L ( ( 1 - P "\r" ) ^ 0 )
  * Lc "}}"
  * ( EOL + -1 )
local MainC =
       EOL
     + Space
     + Tab
     + Escape + EscapeMath
     + CommentLaTeX
     + Beamer
     + DetectedCommands
     + Preproc
     + Comment + LongComment
     + Delim
     + Operator
     + String
     + Punct
     + DefFunction
     + DefClass
     + Type * ( Q ( "*" ) ^ -1 + Space + Punct + Delim + EOL + -1 )
     + Keyword * ( Space + Punct + Delim + EOL + -1 )
     + Builtin * ( Space + Punct + Delim + EOL + -1 )
     + Identifier
     + Number
     + Word
MainLoopC =
  (  ( space^1 * -1 )
     + MainC
  ) ^ 0
languageC =
  Ct (
       ( ( space - P "\r" ) ^0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc '\\__piton_begin_line:'
       * SpaceIndentation ^ 0
       * MainLoopC
       * -1
       * Lc '\\__piton_end_line:'
     )
languages['c'] = languageC
local identifier =
  letter * ( alphanum + P "-" ) ^ 0
  + P '"' * ( ( alphanum + space - P '"' ) ^ 1 ) * P '"'

local Operator =
  K ( 'Operator' ,
      P "=" + P "!=" + P "<>" + P ">=" + P ">" + P "<=" + P "<"  + S "*+/"
    )
local function Set (list)
  local set = {}
  for _, l in ipairs(list) do set[l] = true end
  return set
end

local set_keywords = Set
 {
   "ADD" , "AFTER" , "ALL" , "ALTER" , "AND" , "AS" , "ASC" , "BETWEEN" , "BY" ,
   "CHANGE" , "COLUMN" , "CREATE" , "CROSS JOIN" , "DELETE" , "DESC" , "DISTINCT" ,
   "DROP" , "FROM" , "GROUP" , "HAVING" , "IN" , "INNER" , "INSERT" , "INTO" , "IS" ,
   "JOIN" , "LEFT" , "LIKE" , "LIMIT" , "MERGE" , "NOT" , "NULL" , "ON" , "OR" ,
   "ORDER" , "OVER" , "RIGHT" , "SELECT" , "SET" , "TABLE" , "THEN" , "TRUNCATE" ,
   "UNION" , "UPDATE" , "VALUES" , "WHEN" , "WHERE" , "WITH"
 }

local set_builtins = Set
 {
   "AVG" , "COUNT" , "CHAR_LENGHT" , "CONCAT" , "CURDATE" , "CURRENT_DATE" ,
   "DATE_FORMAT" , "DAY" , "LOWER" , "LTRIM" , "MAX" , "MIN" , "MONTH" , "NOW" ,
   "RANK" , "ROUND" , "RTRIM" , "SUBSTRING" , "SUM" , "UPPER" , "YEAR"
 }
local Identifier =
  C ( identifier ) /
  (
    function (s)
        if set_keywords[string.upper(s)] -- the keywords are case-insensitive in SQL
        then return { "{\\PitonStyle{Keyword}{" } ,
                    { luatexbase.catcodetables.other , s } ,
                    { "}}" }
        else if set_builtins[string.upper(s)]
             then return { "{\\PitonStyle{Name.Builtin}{" } ,
                         { luatexbase.catcodetables.other , s } ,
                         { "}}" }
             else return { "{\\PitonStyle{Name.Field}{" } ,
                         { luatexbase.catcodetables.other , s } ,
                         { "}}" }
             end
        end
    end
  )
local String =
  K ( 'String.Long' , P "'" * ( 1 - P "'" ) ^ 1 * P "'" )
local balanced_braces =
  P { "E" ,
       E =
           (
             P "{" * V "E" * P "}"
             +
             String
             +
             ( 1 - S "{}" )
           ) ^ 0
    }
if piton_beamer
then
  Beamer =
      L ( P "\\pause" * ( P "[" * ( 1 - P "]" ) ^ 0 * P "]" ) ^ -1 )
    +
      Ct ( Cc "Open"
            * C (
                  (
                    P "\\uncover" + P "\\only" + P "\\alert" + P "\\visible"
                    + P "\\invisible" + P "\\action"
                  )
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopSQL:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
    + OneBeamerEnvironment ( "uncoverenv" , MainLoopSQL )
    + OneBeamerEnvironment ( "onlyenv" , MainLoopSQL )
    + OneBeamerEnvironment ( "visibleenv" , MainLoopSQL )
    + OneBeamerEnvironment ( "invisibleenv" , MainLoopSQL )
    + OneBeamerEnvironment ( "alertenv" , MainLoopSQL )
    + OneBeamerEnvironment ( "actionenv" , MainLoopSQL )
    +
      L (
          ( P "\\alt" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
    +
      L (
          ( P "\\temporal" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
end
DetectedCommands =
      Ct ( Cc "Open"
            * C (
                  piton.ListCommands
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopSQL:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
local EOL =
  P "\r"
  *
  (
    ( space^0 * -1 )
    +
    Ct (
         Cc "EOL"
         *
         Ct (
              Lc "\\__piton_end_line:"
              * BeamerEndEnvironments
              * BeamerBeginEnvironments
              * Lc "\\__piton_newline: \\__piton_begin_line:"
            )
       )
  )
  *
  SpaceIndentation ^ 0
local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$"

local Comment =
  WithStyle ( 'Comment' ,
     Q ( P "--" )  -- syntax of SQL92
     * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 )
  * ( EOL + -1 )

local LongComment =
  WithStyle ( 'Comment' ,
               Q ( P "/*" )
               * ( CommentMath + Q ( ( 1 - P "*/" - S "$\r" ) ^ 1 ) + EOL ) ^ 0
               * Q ( P "*/" )
            ) -- $
local CommentLaTeX =
  P(piton.comment_latex)
  * Lc "{\\PitonStyle{Comment.LaTeX}{\\ignorespaces"
  * L ( ( 1 - P "\r" ) ^ 0 )
  * Lc "}}"
  * ( EOL + -1 )
local function LuaKeyword ( name )
return
   Lc ( "{\\PitonStyle{Keyword}{" )
   * Q ( Cmt (
               C ( identifier ) ,
               function(s,i,a) return string.upper(a) == name end
             )
       )
   * Lc ( "}}" )
end
local TableField =
     K ( 'Name.Table' , identifier )
     * Q ( P "." )
     * K ( 'Name.Field' , identifier )

local OneField =
  (
    Q ( P "(" * ( 1 - P ")" ) ^ 0 * P ")" )
    +
    K ( 'Name.Table' , identifier )
      * Q ( P "." )
      * K ( 'Name.Field' , identifier )
    +
    K ( 'Name.Field' , identifier )
  )
  * (
      Space * LuaKeyword ( "AS" ) * Space * K ( 'Name.Field' , identifier )
    ) ^ -1
  * ( Space * ( LuaKeyword ( "ASC" ) + LuaKeyword ( "DESC" ) ) ) ^ -1

local OneTable =
     K ( 'Name.Table' , identifier )
   * (
       Space
       * LuaKeyword ( "AS" )
       * Space
       * K ( 'Name.Table' , identifier )
     ) ^ -1

local WeCatchTableNames =
     LuaKeyword ( "FROM" )
   * ( Space + EOL )
   * OneTable * ( SkipSpace * Q ( P "," ) * SkipSpace * OneTable ) ^ 0
  + (
      LuaKeyword ( "JOIN" ) + LuaKeyword ( "INTO" ) + LuaKeyword ( "UPDATE" )
      + LuaKeyword ( "TABLE" )
    )
    * ( Space + EOL ) * OneTable
local MainSQL =
       EOL
     + Space
     + Tab
     + Escape + EscapeMath
     + CommentLaTeX
     + Beamer
     + DetectedCommands
     + Comment + LongComment
     + Delim
     + Operator
     + String
     + Punct
     + WeCatchTableNames
     + ( TableField + Identifier ) * ( Space + Operator + Punct + Delim + EOL + -1 )
     + Number
     + Word
MainLoopSQL =
  (  ( space^1 * -1 )
     + MainSQL
  ) ^ 0
languageSQL =
  Ct (
       ( ( space - P "\r" ) ^ 0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc '\\__piton_begin_line:'
       * SpaceIndentation ^ 0
       * MainLoopSQL
       * -1
       * Lc '\\__piton_end_line:'
     )
languages['sql'] = languageSQL
local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$"

local Comment =
  WithStyle ( 'Comment' ,
     Q ( P "#" )
     * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 )
  * ( EOL + -1 )

local String =
  WithStyle ( 'String.Short' ,
      Q "\""
      * ( VisualSpace
          + Q ( ( P "\\\"" + 1 - S " \"" ) ^ 1 )
        ) ^ 0
      * Q "\""
    )

local balanced_braces =
  P { "E" ,
       E =
           (
             P "{" * V "E" * P "}"
             +
             String
             +
             ( 1 - S "{}" )
           ) ^ 0
    }

if piton_beamer
then
  Beamer =
      L ( P "\\pause" * ( P "[" * ( 1 - P "]" ) ^ 0 * P "]" ) ^ -1 )
    +
      Ct ( Cc "Open"
            * C (
                  (
                    P "\\uncover" + P "\\only" + P "\\alert" + P "\\visible"
                    + P "\\invisible" + P "\\action"
                  )
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopMinimal:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )
    + OneBeamerEnvironment ( "uncoverenv" , MainLoopMinimal )
    + OneBeamerEnvironment ( "onlyenv" , MainLoopMinimal )
    + OneBeamerEnvironment ( "visibleenv" , MainLoopMinimal )
    + OneBeamerEnvironment ( "invisibleenv" , MainLoopMinimal )
    + OneBeamerEnvironment ( "alertenv" , MainLoopMinimal )
    + OneBeamerEnvironment ( "actionenv" , MainLoopMinimal )
    +
      L (
          ( P "\\alt" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
    +
      L (
          ( P "\\temporal" )
          * P "<" * (1 - P ">") ^ 0 * P ">"
          * P "{"
        )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , balanced_braces )
      * L ( P "}" )
end

DetectedCommands =
      Ct ( Cc "Open"
            * C (
                  piton.ListCommands
                  * ( P "<" * (1 - P ">") ^ 0 * P ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( C ( balanced_braces ) / (function (s) return MainLoopMinimal:match(s) end ) )
       * P "}" * Ct ( Cc "Close" )

local EOL =
  P "\r"
  *
  (
    ( space^0 * -1 )
    +
    Ct (
         Cc "EOL"
         *
         Ct (
              Lc "\\__piton_end_line:"
              * BeamerEndEnvironments
              * BeamerBeginEnvironments
              * Lc "\\__piton_newline: \\__piton_begin_line:"
            )
       )
  )
  *
  SpaceIndentation ^ 0

local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$" -- $

local CommentLaTeX =
  P(piton.comment_latex)
  * Lc "{\\PitonStyle{Comment.LaTeX}{\\ignorespaces"
  * L ( ( 1 - P "\r" ) ^ 0 )
  * Lc "}}"
  * ( EOL + -1 )

local identifier = letter * alphanum ^ 0

local Identifier = K ( 'Identifier' , identifier )

local MainMinimal =
       EOL
     + Space
     + Tab
     + Escape + EscapeMath
     + CommentLaTeX
     + Beamer
     + DetectedCommands
     + Comment
     + Delim
     + String
     + Punct
     + Identifier
     + Number
     + Word

MainLoopMinimal =
  (  ( space^1 * -1 )
     + MainMinimal
  ) ^ 0

languageMinimal =
  Ct (
       ( ( space - P "\r" ) ^ 0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc '\\__piton_begin_line:'
       * SpaceIndentation ^ 0
       * MainLoopMinimal
       * -1
       * Lc '\\__piton_end_line:'
     )
languages['minimal'] = languageMinimal

function piton.Parse(language,code)
  local t = languages[language] : match ( code )
  if t == nil
  then
    tex.sprint("\\PitonSyntaxError")
    return -- to exit in force the function
  end
  local left_stack = {}
  local right_stack = {}
  for _ , one_item in ipairs(t)
  do
     if one_item[1] == "EOL"
     then
          for _ , s in ipairs(right_stack)
            do tex.sprint(s)
            end
          for _ , s in ipairs(one_item[2])
            do tex.tprint(s)
            end
          for _ , s in ipairs(left_stack)
            do tex.sprint(s)
            end
     else
          if one_item[1] == "Open"
          then
               tex.sprint( one_item[2] )
               table.insert(left_stack,one_item[2])
               table.insert(right_stack,one_item[3])
          else
               if one_item[1] == "Close"
               then
                    tex.sprint( right_stack[#right_stack] )
                    left_stack[#left_stack] = nil
                    right_stack[#right_stack] = nil
               else
                    tex.tprint(one_item)
               end
          end
     end
  end
end
function piton.ParseFile(language,name,first_line,last_line)
  local s = ''
  local i = 0
  for line in io.lines(name)
  do i = i + 1
     if i >= first_line
     then s = s .. '\r' .. line
     end
     if i >= last_line then break end
  end
  if string.byte(s,1) == 13
  then if string.byte(s,2) == 239
       then if string.byte(s,3) == 187
            then if string.byte(s,4) == 191
                 then s = string.sub(s,5,-1)
                 end
            end
       end
  end
  piton.Parse(language,s)
end
function piton.ParseBis(language,code)
  local s = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( code )
  return piton.Parse(language,s)
end
function piton.ParseTer(language,code)
  local s = ( Cs ( ( P '\\__piton_breakable_space:' / ' ' + 1 ) ^ 0 ) )
            : match ( code )
  return piton.Parse(language,s)
end
local function gobble(n,code)
  function concat(acc,new_value)
    return acc .. new_value
  end
  if n==0
  then return code
  else
       return Cf (
                   Cc ( "" ) *
                   ( 1 - P "\r" ) ^ (-n)  * C ( ( 1 - P "\r" ) ^ 0 )
                     * ( C ( P "\r" )
                     * ( 1 - P "\r" ) ^ (-n)
                     * C ( ( 1 - P "\r" ) ^ 0 )
                    ) ^ 0 ,
                    concat
                 ) : match ( code )
  end
end
local function add(acc,new_value)
  return acc + new_value
end
local AutoGobbleLPEG =
    ( space ^ 0 * P "\r" ) ^ -1
    * Cf (
           (
             ( P " " ) ^ 0 * P "\r"
             +
             Cf ( Cc(0) * ( P " " * Cc(1) ) ^ 0 , add )
             * ( 1 - P " " ) * ( 1 - P "\r" ) ^ 0 * P "\r"
           ) ^ 0
           *
           ( Cf ( Cc(0) * ( P " " * Cc(1) ) ^ 0 , add )
           * ( 1 - P " " ) * ( 1 - P "\r" ) ^ 0 ) ^ -1 ,
           math.min
         )
local TabsAutoGobbleLPEG =
    ( space ^ 0 * P "\r" ) ^ -1
    * Cf (
           (
             ( P "\t" ) ^ 0 * P "\r"
             +
             Cf ( Cc(0) * ( P "\t" * Cc(1) ) ^ 0 , add )
             * ( 1 - P "\t" ) * ( 1 - P "\r" ) ^ 0 * P "\r"
           ) ^ 0
           *
           ( Cf ( Cc(0) * ( P "\t" * Cc(1) ) ^ 0 , add )
           * ( 1 - P "\t" ) * ( 1 - P "\r" ) ^ 0 ) ^ -1 ,
           math.min
         )
local EnvGobbleLPEG =
  ( ( 1 - P "\r" ) ^ 0 * P "\r" ) ^ 0
    * Cf ( Cc(0) * ( P " " * Cc(1) ) ^ 0 , add ) * -1
function piton.GobbleParse(language,n,code)
  if n==-1
  then n = AutoGobbleLPEG : match(code)
  else if n==-2
       then n = EnvGobbleLPEG : match(code)
       else if n==-3
            then n = TabsAutoGobbleLPEG : match(code)
            end
       end
  end
  piton.Parse(language,gobble(n,code))
  if piton.write ~= ''
  then local file = assert(io.open(piton.write,piton.write_mode))
       file:write(code)
       file:close()
  end
end
function piton.CountLines(code)
  local count = 0
  for i in code : gmatch ( "\r" ) do count = count + 1 end
  tex.sprint(
      luatexbase.catcodetables.expl ,
      '\\int_set:Nn \\l__piton_nb_lines_int {' .. count .. '}' )
end
function piton.CountNonEmptyLines(code)
  local count = 0
  count =
  ( Cf (  Cc(0) *
          (
            ( P " " ) ^ 0 * P "\r"
            + ( 1 - P "\r" ) ^ 0 * P "\r" * Cc(1)
          ) ^ 0
          * (1 - P "\r" ) ^ 0 ,
         add
       ) * -1 ) : match (code)
  tex.sprint(
      luatexbase.catcodetables.expl ,
      '\\int_set:Nn \\l__piton_nb_non_empty_lines_int {' .. count .. '}' )
end
function piton.CountLinesFile(name)
  local count = 0
  io.open(name) -- added
  for line in io.lines(name) do count = count + 1 end
  tex.sprint(
      luatexbase.catcodetables.expl ,
      '\\int_set:Nn \\l__piton_nb_lines_int {' .. count .. '}' )
end
function piton.CountNonEmptyLinesFile(name)
  local count = 0
  for line in io.lines(name)
  do if not ( ( ( P " " ) ^ 0 * -1 ) : match ( line ) )
     then count = count + 1
     end
  end
  tex.sprint(
      luatexbase.catcodetables.expl ,
      '\\int_set:Nn \\l__piton_nb_non_empty_lines_int {' .. count .. '}' )
end
function piton.ComputeRange(marker_beginning,marker_end,file_name)
  local s = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( marker_beginning )
  local t = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( marker_end )
  local first_line = -1
  local count = 0
  local last_found = false
  for line in io.lines(file_name)
  do if first_line == -1
     then if string.sub(line,1,#s) == s
          then first_line = count
          end
     else if string.sub(line,1,#t) == t
          then last_found = true
               break
          end
     end
     count = count + 1
  end
  if first_line == -1
  then tex.sprint("\\PitonBeginMarkerNotFound")
  else if last_found == false
       then tex.sprint("\\PitonEndMarkerNotFound")
       end
  end
  tex.sprint(
      luatexbase.catcodetables.expl ,
      '\\int_set:Nn \\l__piton_first_line_int {' .. first_line .. ' + 2 }'
      .. '\\int_set:Nn \\l__piton_last_line_int {' .. count .. ' }' )
end

