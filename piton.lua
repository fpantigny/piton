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
piton_version = "2.7zzz" -- 2024/03/28


if piton.comment_latex == nil then piton.comment_latex = ">" end
piton.comment_latex = "#" .. piton.comment_latex
function piton.open_brace ()
   tex.sprint("{")
end
function piton.close_brace ()
   tex.sprint("}")
end
local function sprintL3 ( s )
      tex.sprint ( luatexbase.catcodetables.expl , s )
end
local P, S, V, C, Ct, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.C, lpeg.Ct, lpeg.Cc
local Cs , Cg , Cmt , Cb = lpeg.Cs, lpeg.Cg , lpeg.Cmt , lpeg.Cb
local R = lpeg.R
local function Q ( pattern )
  return Ct ( Cc ( luatexbase.catcodetables.CatcodeTableOther ) * C ( pattern ) )
end
local function L ( pattern )
  return Ct ( C ( pattern ) )
end
local function Lc ( string )
  return Cc ( { luatexbase.catcodetables.expl , string } )
end
local function K ( style , pattern )
  return
     Lc ( "{\\PitonStyle{" .. style .. "}{" )
     * Q ( pattern )
     * Lc "}}"
end
local function WithStyle ( style , pattern )
  return
       Ct ( Cc "Open" * Cc ( "{\\PitonStyle{" .. style .. "}{" ) * Cc "}}" )
     * pattern
     * Ct ( Cc "Close" )
end
Escape = P ( false )
EscapeClean = P ( false )
if piton.begin_escape ~= nil
then
  Escape =
    P ( piton.begin_escape )
    * L ( ( 1 - P ( piton.end_escape ) ) ^ 1 )
    * P ( piton.end_escape )
  EscapeClean =
    P ( piton.begin_escape )
    * ( 1 - P ( piton.end_escape ) ) ^ 1
    * P ( piton.end_escape )
end
EscapeMath = P ( false )
if piton.begin_escape_math ~= nil
then
  EscapeMath =
    P ( piton.begin_escape_math )
    * Lc "\\ensuremath{"
    * L ( ( 1 - P(piton.end_escape_math) ) ^ 1 )
    * Lc ( "}" )
    * P ( piton.end_escape_math )
end
lpeg.locale(lpeg)
local alpha , digit = lpeg.alpha , lpeg.digit
local space = P " "
local letter = alpha + "_" + "â" + "à" + "ç" + "é" + "è" + "ê" + "ë" + "ï" + "î"
                    + "ô" + "û" + "ü" + "Â" + "À" + "Ç" + "É" + "È" + "Ê" + "Ë"
                    + "Ï" + "Î" + "Ô" + "Û" + "Ü"

local alphanum = letter + digit
local identifier = letter * alphanum ^ 0
local Identifier = K ( 'Identifier' , identifier )
local Number =
  K ( 'Number' ,
      ( digit ^ 1 * P "." * # ( 1 - P "." ) * digit ^ 0
        + digit ^ 0 * P "." * digit ^ 1
        + digit ^ 1 )
      * ( S "eE" * S "+-" ^ -1 * digit ^ 1 ) ^ -1
      + digit ^ 1
    )
local Word
if piton.begin_escape then
  Word = Q ( ( 1 - space - piton.begin_escape - piton.end_escape
                   - S "'\"\r[({})]" - digit ) ^ 1 )
else
  Word = Q ( ( 1 - space - S "'\"\r[({})]" - digit ) ^ 1 )
end
local Space = Q " " ^ 1

local SkipSpace = Q " " ^ 0

local Punct = Q ( S ".,:;!" )

local Tab = "\t" * Lc "\\l__piton_tab_tl"
local SpaceIndentation = Lc "\\__piton_an_indentation_space:" * Q " "
local Delim = Q ( S "[({})]" )
local VisualSpace = space * Lc "\\l__piton_space_tl"
local LPEG0 = { }
local LPEG1 = { }
local LPEG2 = { }
local LPEG_cleaner = { }
local function Compute_braces ( lpeg_string ) return
    P { "E" ,
         E =
             (
               "{" * V "E" * "}"
               +
               lpeg_string
               +
               ( 1 - S "{}" )
             ) ^ 0
      }
end
local function Compute_DetectedCommands ( lang , braces ) return
  Ct ( Cc "Open"
        * C ( piton.ListCommands * P "{" )
        * Cc "}"
     )
   * ( braces / (function ( s ) return LPEG1[lang] : match ( s ) end ) )
   * P "}"
   * Ct ( Cc "Close" )
end
local function Compute_LPEG_cleaner ( lang , braces ) return
  Ct ( ( piton.ListCommands * "{"
          * (  braces
              / ( function ( s ) return LPEG_cleaner[lang] : match ( s ) end ) )
          * "}"
         + EscapeClean
         +  C ( P ( 1 ) )
        ) ^ 0 ) / table.concat
end
local Beamer = P ( false )
local BeamerBeginEnvironments = P ( true )
local BeamerEndEnvironments = P ( true )
local list_beamer_env =
  { "uncoverenv" , "onlyenv" , "visibleenv" ,
    "invisibleenv" , "alertenv" , "actionenv" }
local BeamerNamesEnvironments = P ( false )
for _ , x in ipairs ( list_beamer_env ) do
  BeamerNamesEnvironments = BeamerNamesEnvironments + x
end
BeamerBeginEnvironments =
    ( space ^ 0 *
      L
        (
          P "\\begin{" * BeamerNamesEnvironments * "}"
          * ( "<" * ( 1 - P ">" ) ^ 0 * ">" ) ^ -1
        )
      * "\r"
    ) ^ 0
BeamerEndEnvironments =
    ( space ^ 0 *
      L ( P "\\end{" * BeamerNamesEnvironments * "}" )
      * "\r"
    ) ^ 0
local function Compute_Beamer ( lang , braces )
  local lpeg = L ( P "\\pause" * ( "[" * ( 1 - P "]" ) ^ 0 * "]" ) ^ -1 )
  lpeg = lpeg +
      Ct ( Cc "Open"
            * C ( ( P "\\uncover" + "\\only" + "\\alert" + "\\visible"
                    + "\\invisible" + "\\action" )
                  * ( "<" * ( 1 - P ">" ) ^ 0 * ">" ) ^ -1
                  * P "{"
                )
            * Cc "}"
         )
       * ( braces / ( function ( s ) return LPEG1[lang] : match ( s ) end ) )
       * "}"
       * Ct ( Cc "Close" )
  lpeg = lpeg +
    L ( P "\\alt" * "<" * ( 1 - P ">" ) ^ 0 * ">" * "{" )
     * K ( 'ParseAgain.noCR' , braces )
     * L ( P "}{" )
     * K ( 'ParseAgain.noCR' , braces )
     * L ( P "}" )
  lpeg = lpeg +
      L ( ( P "\\temporal" ) * "<" * ( 1 - P ">" ) ^ 0 * ">" * "{" )
      * K ( 'ParseAgain.noCR' , braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , braces )
      * L ( P "}{" )
      * K ( 'ParseAgain.noCR' , braces )
      * L ( P "}" )
  for _ , x in ipairs ( list_beamer_env ) do
  lpeg = lpeg +
        Ct ( Cc "Open"
             * C (
                    P ( "\\begin{" .. x .. "}" )
                    * ( "<" * ( 1 - P ">") ^ 0 * ">" ) ^ -1
                  )
             * Cc ( "\\end{" .. x ..  "}" )
            )
        * (
            ( ( 1 - P ( "\\end{" .. x .. "}" ) ) ^ 0 )
                / ( function ( s ) return LPEG1[lang] : match ( s ) end )
          )
        * P ( "\\end{" .. x .. "}" )
        * Ct ( Cc "Close" )
  end
  return lpeg
end
local CommentMath =
  P "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1  ) * P "$" -- $
local PromptHastyDetection =
  ( # ( P ">>>" + "..." ) * Lc '\\__piton_prompt:' ) ^ -1
local Prompt = K ( 'Prompt' , ( ( P ">>>" + "..." ) * P " " ^ -1 ) ^ -1  )
local EOL =
  P "\r"
  *
  (
    ( space ^ 0 * -1 )
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
  * ( SpaceIndentation ^ 0 * # ( 1 - S " \r" ) ) ^ -1
local CommentLaTeX =
  P(piton.comment_latex)
  * Lc "{\\PitonStyle{Comment.LaTeX}{\\ignorespaces"
  * L ( ( 1 - P "\r" ) ^ 0 )
  * Lc "}}"
  * ( EOL + -1 )
local Operator =
  K ( 'Operator' ,
      P "!=" + "<>" + "==" + "<<" + ">>" + "<=" + ">=" + ":=" + "//" + "**"
      + S "-~+/*%=<>&.@|" )

local OperatorWord =
  K ( 'Operator.Word' , P "in" + "is" + "and" + "or" + "not" )

local Keyword =
  K ( 'Keyword' ,
      P "as" + "assert" + "break" + "case" + "class" + "continue" + "def" +
      "del" + "elif" + "else" + "except" + "exec" + "finally" + "for" + "from" +
      "global" + "if" + "import" + "lambda" + "non local" + "pass" + "return" +
      "try" + "while" + "with" + "yield" + "yield from" )
  + K ( 'Keyword.Constant' , P "True" + "False" + "None" )

local Builtin =
  K ( 'Name.Builtin' ,
      P "__import__" + "abs" + "all" + "any" + "bin" + "bool" + "bytearray" +
      "bytes" + "chr" + "classmethod" + "compile" + "complex" + "delattr" +
      "dict" + "dir" + "divmod" + "enumerate" + "eval" + "filter" + "float" +
      "format" + "frozenset" + "getattr" + "globals" + "hasattr" + "hash" +
      "hex" + "id" + "input" + "int" + "isinstance" + "issubclass" + "iter" +
      "len" + "list" + "locals" + "map" + "max" + "memoryview" + "min" + "next"
      + "object" + "oct" + "open" + "ord" + "pow" + "print" + "property" +
      "range" + "repr" + "reversed" + "round" + "set" + "setattr" + "slice" +
      "sorted" + "staticmethod" + "str" + "sum" + "super" + "tuple" + "type" +
      "vars" + "zip" )

local Exception =
  K ( 'Exception' ,
      P "ArithmeticError" + "AssertionError" + "AttributeError" +
      "BaseException" + "BufferError" + "BytesWarning" + "DeprecationWarning" +
      "EOFError" + "EnvironmentError" + "Exception" + "FloatingPointError" +
      "FutureWarning" + "GeneratorExit" + "IOError" + "ImportError" +
      "ImportWarning" + "IndentationError" + "IndexError" + "KeyError" +
      "KeyboardInterrupt" + "LookupError" + "MemoryError" + "NameError" +
      "NotImplementedError" + "OSError" + "OverflowError" +
      "PendingDeprecationWarning" + "ReferenceError" + "ResourceWarning" +
      "RuntimeError" + "RuntimeWarning" + "StopIteration" + "SyntaxError" +
      "SyntaxWarning" + "SystemError" + "SystemExit" + "TabError" + "TypeError"
      + "UnboundLocalError" + "UnicodeDecodeError" + "UnicodeEncodeError" +
      "UnicodeError" + "UnicodeTranslateError" + "UnicodeWarning" +
      "UserWarning" + "ValueError" + "VMSError" + "Warning" + "WindowsError" +
      "ZeroDivisionError" + "BlockingIOError" + "ChildProcessError" +
      "ConnectionError" + "BrokenPipeError" + "ConnectionAbortedError" +
      "ConnectionRefusedError" + "ConnectionResetError" + "FileExistsError" +
      "FileNotFoundError" + "InterruptedError" + "IsADirectoryError" +
      "NotADirectoryError" + "PermissionError" + "ProcessLookupError" +
      "TimeoutError" + "StopAsyncIteration" + "ModuleNotFoundError" +
      "RecursionError" )

local RaiseException = K ( 'Keyword' , P "raise" ) * SkipSpace * Exception * Q "("

local Decorator = K ( 'Name.Decorator' , P "@" * letter ^ 1  )
local DefClass =
  K ( 'Keyword' , "class" ) * Space * K ( 'Name.Class' , identifier )
local ImportAs =
  K ( 'Keyword' , "import" )
   * Space
   * K ( 'Name.Namespace' , identifier * ( "." * identifier ) ^ 0 )
   * (
       ( Space * K ( 'Keyword' , "as" ) * Space
          * K ( 'Name.Namespace' , identifier ) )
       +
       ( SkipSpace * Q "," * SkipSpace
          * K ( 'Name.Namespace' , identifier ) ) ^ 0
     )
local FromImport =
  K ( 'Keyword' , "from" )
    * Space * K ( 'Name.Namespace' , identifier )
    * Space * K ( 'Keyword' , "import" )
local PercentInterpol =
  K ( 'String.Interpol' ,
      P "%"
      * ( "(" * alphanum ^ 1 * ")" ) ^ -1
      * ( S "-#0 +" ) ^ 0
      * ( digit ^ 1 + "*" ) ^ -1
      * ( "." * ( digit ^ 1 + "*" ) ) ^ -1
      * ( S "HlL" ) ^ -1
      * S "sdfFeExXorgiGauc%"
    )
local SingleShortString =
  WithStyle ( 'String.Short' ,
         Q ( P "f'" + "F'" )
         * (
             K ( 'String.Interpol' , "{" )
              * K ( 'Interpol.Inside' , ( 1 - S "}':" ) ^ 0  )
              * Q ( P ":" * ( 1 - S "}:'" ) ^ 0 ) ^ -1
              * K ( 'String.Interpol' , "}" )
             +
             VisualSpace
             +
             Q ( ( P "\\'" + "{{" + "}}" + 1 - S " {}'" ) ^ 1 )
           ) ^ 0
         * Q "'"
       +
         Q ( P "'" + "r'" + "R'" )
         * ( Q ( ( P "\\'" + 1 - S " '\r%" ) ^ 1 )
             + VisualSpace
             + PercentInterpol
             + Q "%"
           ) ^ 0
         * Q "'" )

local DoubleShortString =
  WithStyle ( 'String.Short' ,
         Q ( P "f\"" + "F\"" )
         * (
             K ( 'String.Interpol' , "{" )
               * K ( 'Interpol.Inside' , ( 1 - S "}\":" ) ^ 0 )
               * ( K ( 'String.Interpol' , ":" ) * Q ( (1 - S "}:\"") ^ 0 ) ) ^ -1
               * K ( 'String.Interpol' , "}" )
             +
             VisualSpace
             +
             Q ( ( P "\\\"" + "{{" + "}}" + 1 - S " {}\"" ) ^ 1 )
            ) ^ 0
         * Q "\""
       +
         Q ( P "\"" + "r\"" + "R\"" )
         * ( Q ( ( P "\\\"" + 1 - S " \"\r%" ) ^ 1 )
             + VisualSpace
             + PercentInterpol
             + Q "%"
           ) ^ 0
         * Q "\""  )

local ShortString = SingleShortString + DoubleShortString
local braces = Compute_braces ( ShortString )
if piton.beamer then Beamer = Compute_Beamer ( 'python' , braces ) end
DetectedCommands = Compute_DetectedCommands ( 'python' , braces )
LPEG_cleaner['python'] = Compute_LPEG_cleaner ( 'python' , braces )
local SingleLongString =
  WithStyle ( 'String.Long' ,
     ( Q ( S "fF" * P "'''" )
         * (
             K ( 'String.Interpol' , "{" )
               * K ( 'Interpol.Inside' , ( 1 - S "}:\r" - "'''" ) ^ 0  )
               * Q ( P ":" * (1 - S "}:\r" - "'''" ) ^ 0 ) ^ -1
               * K ( 'String.Interpol' , "}"  )
             +
             Q ( ( 1 - P "'''" - S "{}'\r" ) ^ 1 )
             +
             EOL
           ) ^ 0
       +
         Q ( ( S "rR" ) ^ -1  * "'''" )
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
      * Q "'''"  )

local DoubleLongString =
  WithStyle ( 'String.Long' ,
     (
        Q ( S "fF" * "\"\"\"" )
        * (
            K ( 'String.Interpol', "{"  )
              * K ( 'Interpol.Inside' , ( 1 - S "}:\r" - "\"\"\"" ) ^ 0 )
              * Q ( ":" * (1 - S "}:\r" - "\"\"\"" ) ^ 0 ) ^ -1
              * K ( 'String.Interpol' , "}"  )
            +
            Q ( ( 1 - S "{}\"\r" - "\"\"\"" ) ^ 1 )
            +
            EOL
          ) ^ 0
      +
        Q ( S "rR" ^ -1  * "\"\"\"" )
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
     * Q "\"\"\""
  )
local LongString = SingleLongString + DoubleLongString
local StringDoc =
    K ( 'String.Doc' , P "r" ^ -1 * "\"\"\"" )
      * ( K ( 'String.Doc' , (1 - P "\"\"\"" - "\r" ) ^ 0  ) * EOL
          * Tab ^ 0
        ) ^ 0
      * K ( 'String.Doc' , ( 1 - P "\"\"\"" - "\r" ) ^ 0 * "\"\"\"" )
local Comment =
  WithStyle ( 'Comment' ,
     Q "#" * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 ) -- $
           * ( EOL + -1 )
local expression =
  P { "E" ,
       E = ( "'" * ( P "\\'" + 1 - S "'\r" ) ^ 0 * "'"
             + "\"" * ( P "\\\"" + 1 - S "\"\r" ) ^ 0 * "\""
             + "{" * V "F" * "}"
             + "(" * V "F" * ")"
             + "[" * V "F" * "]"
             + ( 1 - S "{}()[]\r," ) ) ^ 0 ,
       F = (   "{" * V "F" * "}"
             + "(" * V "F" * ")"
             + "[" * V "F" * "]"
             + ( 1 - S "{}()[]\r\"'" ) ) ^ 0
    }
local Params =
  P { "E" ,
       E = ( V "F" * ( Q "," * V "F" ) ^ 0 ) ^ -1 ,
       F = SkipSpace * ( Identifier + Q "*args" + Q "**kwargs" ) * SkipSpace
           * (
                 K ( 'InitialValues' , "=" * expression )
               + Q ":" * SkipSpace * K ( 'Name.Type' , identifier )
             ) ^ -1
    }
local DefFunction =
  K ( 'Keyword' , "def" )
  * Space
  * K ( 'Name.Function.Internal' , identifier )
  * SkipSpace
  * Q "("  * Params * Q ")"
  * SkipSpace
  * ( Q "->" * SkipSpace * K ( 'Name.Type' , identifier ) ) ^ -1
  * K ( 'ParseAgain.noCR' , ( 1 - S ":\r" ) ^ 0 )
  * Q ":"
  * ( SkipSpace
      * ( EOL + CommentLaTeX + Comment ) -- in all cases, that contains an EOL
      * Tab ^ 0
      * SkipSpace
      * StringDoc ^ 0 -- there may be additionnal docstrings
    ) ^ -1
local ExceptionInConsole = Exception *  Q ( ( 1 - P "\r" ) ^ 0 ) * EOL
local Main =
       space ^ 1 * -1
     + space ^ 0 * EOL
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
LPEG1['python'] = Main ^ 0
LPEG2['python'] =
  Ct (
       ( space ^ 0 * "\r" ) ^ -1
       * BeamerBeginEnvironments
       * PromptHastyDetection
       * Lc '\\__piton_begin_line:'
       * Prompt
       * SpaceIndentation ^ 0
       * LPEG1['python']
       * -1
       * Lc '\\__piton_end_line:'
     )
local Delim = Q ( P "[|" + "|]" + S "[()]" )
local Punct = Q ( S ",:;!" )
local cap_identifier = R "AZ" * ( R "az" + R "AZ" + S "_'" + digit ) ^ 0
local Constructor = K ( 'Name.Constructor' , cap_identifier )
local ModuleType = K ( 'Name.Type' , cap_identifier )
local identifier = ( R "az" + "_" ) * ( R "az" + R "AZ" + S "_'" + digit ) ^ 0
local Identifier = K ( 'Identifier' , identifier )
local expression_for_fields =
  P { "E" ,
       E = (   "{" * V "F" * "}"
             + "(" * V "F" * ")"
             + "[" * V "F" * "]"
             + "\"" * ( P "\\\"" + 1 - S "\"\r" ) ^ 0 * "\""
             + "'" * ( P "\\'" + 1 - S "'\r" ) ^ 0 * "'"
             + ( 1 - S "{}()[]\r;" ) ) ^ 0 ,
       F = (   "{" * V "F" * "}"
             + "(" * V "F" * ")"
             + "[" * V "F" * "]"
             + ( 1 - S "{}()[]\r\"'" ) ) ^ 0
    }
local OneFieldDefinition =
    ( K ( 'Keyword' , "mutable" ) * SkipSpace ) ^ -1
  * K ( 'Name.Field' , identifier ) * SkipSpace
  * Q ":" * SkipSpace
  * K ( 'Name.Type' , expression_for_fields )
  * SkipSpace

local OneField =
    K ( 'Name.Field' , identifier ) * SkipSpace
  * Q "=" * SkipSpace
  * ( expression_for_fields
      / ( function ( s ) return LPEG1['ocaml'] : match ( s ) end )
    )
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
      P "!=" + "<>" + "==" + "<<" + ">>" + "<=" + ">=" + ":=" + "||" + "&&" +
      "//" + "**" + ";;" + "::" + "->" + "+." + "-." + "*." + "/."
      + S "-~+/*%=<>&@|" )

local OperatorWord =
  K ( 'Operator.Word' ,
      P "and" + "asr" + "land" + "lor" + "lsl" + "lxor" + "mod" + "or" )

local Keyword =
  K ( 'Keyword' ,
      P "assert" + "and" + "as" + "begin" + "class" + "constraint" + "done"
  + "downto" + "do" + "else" + "end" + "exception" + "external" + "for" +
  "function" + "functor" + "fun" + "if" + "include" + "inherit" + "initializer"
  + "in"  + "lazy" + "let" + "match" + "method" + "module" + "mutable" + "new" +
  "object" + "of" + "open" + "private" + "raise" + "rec" + "sig" + "struct" +
  "then" + "to" + "try" + "type" + "value" + "val" + "virtual" + "when" +
  "while" + "with" )
  + K ( 'Keyword.Constant' , P "true" + "false" )

local Builtin =
  K ( 'Name.Builtin' , P "not" + "incr" + "decr" + "fst" + "snd" )
local Exception =
  K (   'Exception' ,
       P "Division_by_zero" + "End_of_File" + "Failure" + "Invalid_argument" +
       "Match_failure" + "Not_found" + "Out_of_memory" + "Stack_overflow" +
       "Sys_blocked_io" + "Sys_error" + "Undefined_recursive_module" )
local Char =
  K ( 'String.Short' , "'" * ( ( 1 - P "'" ) ^ 0 + "\\'" ) * "'" )
braces = Compute_braces ( "\"" * ( 1 - S "\"" ) ^ 0 * "\"" )
if piton.beamer then
  Beamer = Compute_Beamer ( 'ocaml' , "\"" * ( 1 - S "\"" ) ^ 0 * "\"" )
end
DetectedCommands = Compute_DetectedCommands ( 'ocaml' , braces )
LPEG_cleaner['ocaml'] = Compute_LPEG_cleaner ( 'ocaml' , braces )
local ocaml_string =
       Q "\""
     * (
         VisualSpace
         +
         Q ( ( 1 - S " \"\r" ) ^ 1 )
         +
         EOL
       ) ^ 0
     * Q "\""
local String = WithStyle ( 'String.Long' , ocaml_string )
local ext = ( R "az" + "_" ) ^ 0
local open = "{" * Cg ( ext , 'init' ) * "|"
local close = "|" * C ( ext ) * "}"
local closeeq =
  Cmt ( close * Cb ( 'init' ) ,
        function ( s , i , a , b ) return a == b end )
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
  ( function ( s ) return QuotedStringBis : match ( s ) end )
local Comment =
  WithStyle ( 'Comment' ,
     P {
         "A" ,
         A = Q "(*"
             * ( V "A"
                 + Q ( ( 1 - S "\r$\"" - "(*" - "*)" ) ^ 1 ) -- $
                 + ocaml_string
                 + "$" * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1 ) * "$" -- $
                 + EOL
               ) ^ 0
             * Q "*)"
       }   )
local balanced_parens =
  P { "E" , E = ( "(" * V "E" * ")" + 1 - S "()" ) ^ 0 }
local Argument =
  K ( 'Identifier' , identifier )
  + Q "(" * SkipSpace
    * K ( 'Identifier' , identifier ) * SkipSpace
    * Q ":" * SkipSpace
    * K ( 'Name.Type' , balanced_parens ) * SkipSpace
    * Q ")"
local DefFunction =
  K ( 'Keyword' , "let open" )
   * Space
   * K ( 'Name.Module' , cap_identifier )
  +
  K ( 'Keyword' , P "let rec" + "let" + "and" )
    * Space
    * K ( 'Name.Function.Internal' , identifier )
    * Space
    * (
        Q "=" * SkipSpace * K ( 'Keyword' , "function" )
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
  K ( 'Keyword' , "module" ) * Space
  *
    (
          K ( 'Keyword' , "type" ) * Space
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
  K ( 'Keyword' , P "include" + "open" )
  * Space * K ( 'Name.Module' , cap_identifier )
local TypeParameter = K ( 'TypeParameter' , "'" * alpha * # ( 1 - P "'" ) )
local Main =
       space ^ 1 * -1
     + space ^ 0 * EOL
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

LPEG1['ocaml'] = Main ^ 0
LPEG2['ocaml'] =
  Ct (
       ( space ^ 0 * "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc '\\__piton_begin_line:'
       * SpaceIndentation ^ 0
       * LPEG1['ocaml']
       * -1
       * Lc '\\__piton_end_line:'
     )
local Delim = Q ( S "{[()]}" )
local Punct = Q ( S ",:;!" )
local identifier = letter * alphanum ^ 0

local Operator =
  K ( 'Operator' ,
      P "!=" + "==" + "<<" + ">>" + "<=" + ">=" + "||" + "&&"
        + S "-~+/*%=<>&.@|!" )

local Keyword =
  K ( 'Keyword' ,
      P "alignas" + "asm" + "auto" + "break" + "case" + "catch" + "class" +
      "const" + "constexpr" + "continue" + "decltype" + "do" + "else" + "enum" +
      "extern" + "for" + "goto" + "if" + "nexcept" + "private" + "public" +
      "register" + "restricted" + "return" + "static" + "static_assert" +
      "struct" + "switch" + "thread_local" + "throw" + "try" + "typedef" +
      "union" + "using" + "virtual" + "volatile" + "while"
    )
  + K ( 'Keyword.Constant' , P "default" + "false" + "NULL" + "nullptr" + "true" )

local Builtin =
  K ( 'Name.Builtin' ,
      P "alignof" + "malloc" + "printf" + "scanf" + "sizeof" )

local Type =
  K ( 'Name.Type' ,
      P "bool" + "char" + "char16_t" + "char32_t" + "double" + "float" + "int" +
      "int8_t" + "int16_t" + "int32_t" + "int64_t" + "long" + "short" + "signed"
      + "unsigned" + "void" + "wchar_t" )

local DefFunction =
  Type
  * Space
  * Q "*" ^ -1
  * K ( 'Name.Function.Internal' , identifier )
  * SkipSpace
  * # P "("
local DefClass =
  K ( 'Keyword' , "class" ) * Space * K ( 'Name.Class' , identifier )
String =
  WithStyle ( 'String.Long' ,
      Q "\""
      * ( VisualSpace
          + K ( 'String.Interpol' ,
                "%" * ( S "difcspxXou" + "ld" + "li" + "hd" + "hi" )
              )
          + Q ( ( P "\\\"" + 1 - S " \"" ) ^ 1 )
        ) ^ 0
      * Q "\""
    )
braces = Compute_braces ( "\"" * ( 1 - S "\"" ) ^ 0 * "\"" )
if piton.beamer then Beamer = Compute_Beamer ( 'c' , braces ) end
DetectedCommands = Compute_DetectedCommands ( 'c' , braces )
LPEG_cleaner['c'] = Compute_LPEG_cleaner ( 'c' , braces )
local Preproc = K ( 'Preproc' , "#" * ( 1 - P "\r" ) ^ 0  ) * ( EOL + -1 )
local Comment =
  WithStyle ( 'Comment' ,
     Q "//" * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 ) -- $
            * ( EOL + -1 )

local LongComment =
  WithStyle ( 'Comment' ,
               Q "/*"
               * ( CommentMath + Q ( ( 1 - P "*/" - S "$\r" ) ^ 1 ) + EOL ) ^ 0
               * Q "*/"
            ) -- $
local Main =
       space ^ 1 * -1
     + space ^ 0 * EOL
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
     + Type * ( Q "*" ^ -1 + Space + Punct + Delim + EOL + -1 )
     + Keyword * ( Space + Punct + Delim + EOL + -1 )
     + Builtin * ( Space + Punct + Delim + EOL + -1 )
     + Identifier
     + Number
     + Word
LPEG1['c'] = Main ^ 0
LPEG2['c'] =
  Ct (
       ( space ^ 0 * P "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc '\\__piton_begin_line:'
       * SpaceIndentation ^ 0
       * LPEG1['c']
       * -1
       * Lc '\\__piton_end_line:'
     )
local function LuaKeyword ( name )
return
   Lc [[{\PitonStyle{Keyword}{]]
   * Q ( Cmt (
               C ( identifier ) ,
               function ( s , i , a ) return string.upper ( a ) == name end
             )
       )
   * Lc "}}"
end
local identifier =
  letter * ( alphanum + "-" ) ^ 0
  + '"' * ( ( alphanum + space - '"' ) ^ 1 ) * '"'

local Operator =
  K ( 'Operator' , P "=" + "!=" + "<>" + ">=" + ">" + "<=" + "<"  + S "*+/" )
local function Set ( list )
  local set = { }
  for _, l in ipairs ( list ) do set[l] = true end
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
local String = K ( 'String.Long' , "'" * ( 1 - P "'" ) ^ 1 * "'" )
braces = Compute_braces ( String )
if piton.beamer then Beamer = Compute_Beamer ( 'sql' , braces ) end
DetectedCommands = Compute_DetectedCommands ( 'sql' , braces )
LPEG_cleaner['sql'] = Compute_LPEG_cleaner ( 'sql' , braces )
local Comment =
  WithStyle ( 'Comment' ,
     Q "--"   -- syntax of SQL92
     * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 ) -- $
  * ( EOL + -1 )

local LongComment =
  WithStyle ( 'Comment' ,
               Q "/*"
               * ( CommentMath + Q ( ( 1 - P "*/" - S "$\r" ) ^ 1 ) + EOL ) ^ 0
               * Q "*/"
            ) -- $
local TableField =
       K ( 'Name.Table' , identifier )
     * Q "."
     * K ( 'Name.Field' , identifier )

local OneField =
  (
    Q ( "(" * ( 1 - P ")" ) ^ 0 * ")" )
    +
        K ( 'Name.Table' , identifier )
      * Q "."
      * K ( 'Name.Field' , identifier )
    +
    K ( 'Name.Field' , identifier )
  )
  * (
      Space * LuaKeyword "AS" * Space * K ( 'Name.Field' , identifier )
    ) ^ -1
  * ( Space * ( LuaKeyword "ASC" + LuaKeyword "DESC" ) ) ^ -1

local OneTable =
     K ( 'Name.Table' , identifier )
   * (
       Space
       * LuaKeyword "AS"
       * Space
       * K ( 'Name.Table' , identifier )
     ) ^ -1

local WeCatchTableNames =
     LuaKeyword "FROM"
   * ( Space + EOL )
   * OneTable * ( SkipSpace * Q "," * SkipSpace * OneTable ) ^ 0
  + (
      LuaKeyword "JOIN" + LuaKeyword "INTO" + LuaKeyword "UPDATE"
      + LuaKeyword "TABLE"
    )
    * ( Space + EOL ) * OneTable
local Main =
       space ^ 1 * -1
     + space ^ 0 * EOL
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
LPEG1['sql'] = Main ^ 0
LPEG2['sql'] =
  Ct (
       ( space ^ 0 * "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc [[ \__piton_begin_line: ]]
       * SpaceIndentation ^ 0
       * LPEG1['sql']
       * -1
       * Lc [[ \__piton_end_line: ]]
     )
local Punct = Q ( S ",:;!\\" )

local Comment =
  WithStyle ( 'Comment' ,
              Q "#"
              * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 -- $
            )
     * ( EOL + -1 )

local String =
  WithStyle ( 'String.Short' ,
              Q "\""
              * ( VisualSpace
                  + Q ( ( P "\\\"" + 1 - S " \"" ) ^ 1 )
                ) ^ 0
              * Q "\""
            )

braces = Compute_braces ( String )
if piton.beamer then Beamer = Compute_Beamer ( 'minimal' , braces ) end

DetectedCommands = Compute_DetectedCommands ( 'minimal' , braces )

LPEG_cleaner['minimal'] = Compute_LPEG_cleaner ( 'minimal' , braces )

local identifier = letter * alphanum ^ 0

local Identifier = K ( 'Identifier' , identifier )

local Delim = Q ( S "{[()]}" )

local Main =
       space ^ 1 * -1
     + space ^ 0 * EOL
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

LPEG1['minimal'] = Main ^ 0

LPEG2['minimal'] =
  Ct (
       ( space ^ 0 * "\r" ) ^ -1
       * BeamerBeginEnvironments
       * Lc [[ \__piton_begin_line: ]]
       * SpaceIndentation ^ 0
       * LPEG1['minimal']
       * -1
       * Lc [[ \__piton_end_line: ]]
     )

function piton.Parse ( language , code )
  local t = LPEG2[language] : match ( code )
  if t == nil
  then
    sprintL3 [[ \__piton_error_or_warning:n { syntax~error } ]]
    return -- to exit in force the function
  end
  local left_stack = {}
  local right_stack = {}
  for _ , one_item in ipairs ( t ) do
    if one_item[1] == "EOL" then
      for _ , s in ipairs ( right_stack ) do
        tex.sprint ( s )
      end
      for _ , s in ipairs ( one_item[2] ) do
        tex.tprint ( s )
      end
      for _ , s in ipairs ( left_stack ) do
        tex.sprint ( s )
      end
    else
      if one_item[1] == "Open" then
        tex.sprint( one_item[2] )
        table.insert ( left_stack , one_item[2] )
        table.insert ( right_stack , one_item[3] )
      else
        if one_item[1] == "Close" then
          tex.sprint ( right_stack[#right_stack] )
          left_stack[#left_stack] = nil
          right_stack[#right_stack] = nil
        else
          tex.tprint ( one_item )
        end
      end
    end
  end
end
function piton.ParseFile ( language , name , first_line , last_line , split )
  local s = ''
  local i = 0
  for line in io.lines ( name ) do
    i = i + 1
    if i >= first_line then
      s = s .. '\r' .. line
    end
    if i >= last_line then break end
  end
  if string.byte ( s , 1 ) == 13 then
    if string.byte ( s , 2 ) == 239 then
      if string.byte ( s , 3 ) == 187 then
        if string.byte ( s , 4 ) == 191 then
          s = string.sub ( s , 5 , -1 )
        end
      end
    end
  end
  if split == 1 then
    piton.GobbleSplitParse ( language , 0 , s )
  else
    sprintL3 [[ \bool_if:NT \g__piton_footnote_bool \savenotes \vtop \bgroup ]]
    piton.Parse ( language , s )
    sprintL3
      [[\vspace{2.5pt}\egroup\bool_if:NT\g__piton_footnote_bool\endsavenotes\par]]
  end
end
function piton.ParseBis ( lang , code )
  local s = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( code )
  return piton.Parse ( lang , s )
end
function piton.ParseTer ( lang , code )
  local s = ( Cs ( ( P [[\__piton_breakable_space: ]] / ' ' + 1 ) ^ 0 ) )
            : match ( code )
  return piton.Parse ( lang , s )
end
local AutoGobbleLPEG =
      (  (
           P " " ^ 0 * "\r"
           +
           Ct ( C " " ^ 0 ) / table.getn
           * ( 1 - P " " ) * ( 1 - P "\r" ) ^ 0 * "\r"
         ) ^ 0
         * ( Ct ( C " " ^ 0 ) / table.getn
              * ( 1 - P " " ) * ( 1 - P "\r" ) ^ 0 ) ^ -1
       ) / math.min
local TabsAutoGobbleLPEG =
       (
         (
           P "\t" ^ 0 * "\r"
           +
           Ct ( C "\t" ^ 0 ) / table.getn
           * ( 1 - P "\t" ) * ( 1 - P "\r" ) ^ 0 * "\r"
         ) ^ 0
         * ( Ct ( C "\t" ^ 0 ) / table.getn
             * ( 1 - P "\t" ) * ( 1 - P "\r" ) ^ 0 ) ^ -1
       ) / math.min
local EnvGobbleLPEG =
      ( ( 1 - P "\r" ) ^ 0 * "\r" ) ^ 0
    * Ct ( C " " ^ 0 * -1 ) / table.getn
local function remove_before_cr ( input_string )
    local match_result = ( P "\r" ) : match ( input_string )
    if match_result then
      return string.sub ( input_string , match_result )
    else
      return input_string
    end
end
local function gobble ( n , code )
  code = remove_before_cr ( code )
  if n == 0 then
    return code
  else
    if n == -1 then
      n = AutoGobbleLPEG : match ( code )
    else
      if n == -2 then
        n = EnvGobbleLPEG : match ( code )
      else
        if n == -3 then
          n = TabsAutoGobbleLPEG : match ( code )
        end
      end
    end
    if n == 0 then
      return code
    else
      return
      ( Ct (
             ( 1 - P "\r" ) ^ (-n) * C ( ( 1 - P "\r" ) ^ 0 )
               * ( C "\r" * ( 1 - P "\r" ) ^ (-n) * C ( ( 1 - P "\r" ) ^ 0 )
           ) ^ 0 )
        / table.concat
      ) : match ( code )
    end
  end
end
function piton.GobbleParse ( lang , n , code )
  piton.last_code = gobble ( n , code )
  piton.last_language = lang
  sprintL3 [[ \bool_if:NT \g__piton_footnote_bool \savenotes \vtop \bgroup ]]
  piton.Parse ( lang , piton.last_code )
  sprintL3
    [[\vspace{2.5pt}\egroup\bool_if:NT\g__piton_footnote_bool\endsavenotes\par]]
  if piton.write and piton.write ~= '' then
    local file = assert ( io.open ( piton.write , piton.write_mode ) )
    file:write ( piton.get_last_code ( ) )
    file:close ( )
  end
end
function piton.GobbleSplitParse ( lang , n , code )
  P { "E" ,
      E = ( V "F"
           * ( P " " ^ 0 * "\r"
               / ( function ( x ) sprintL3 [[ \__piton_incr_visual_line: ]] end )
             ) ^ 1
             / ( function ( x )
                 sprintL3 [[ \l__piton_split_separation_tl \int_gzero:N \g__piton_line_int ]]
                 end )
          ) ^ 0 * V "F" ,
      F = C ( V "G" ^ 0 )
          / ( function ( x ) piton.GobbleParse ( lang , 0 , x ) end ) ,
      G = ( 1 - P "\r" ) ^ 0 * "\r" - ( P " " ^ 0 * "\r" )
    } : match ( gobble ( n , code ) )
end
function piton.get_last_code ( )
  return LPEG_cleaner[piton.last_language] : match ( piton.last_code )
end
function piton.CountLines ( code )
  local count = 0
  for i in code : gmatch ( "\r" ) do count = count + 1 end
  sprintL3 ( [[ \int_set:Nn  \l__piton_nb_lines_int { ]] .. count .. '}' )
end
function piton.CountNonEmptyLines ( code )
  local count = 0
  count =
     ( Ct ( ( P " " ^ 0 * "\r"
              + ( 1 - P "\r" ) ^ 0 * C "\r" ) ^ 0
            * ( 1 - P "\r" ) ^ 0
            * -1
          ) / table.getn
     ) : match ( code )
  sprintL3 ( [[ \int_set:Nn  \l__piton_nb_non_empty_lines_int { ]] .. count .. '}' )
end
function piton.CountLinesFile ( name )
  local count = 0
  for line in io.lines ( name ) do count = count + 1 end
  sprintL3 ( [[ \int_set:Nn \l__piton_nb_lines_int { ]] .. count .. '}' )
end
function piton.CountNonEmptyLinesFile ( name )
  local count = 0
  for line in io.lines ( name )
  do if not ( ( P " " ^ 0 * -1 ) : match ( line ) ) then
       count = count + 1
     end
  end
  sprintL3 ( [[ \int_set:Nn \l__piton_nb_non_empty_lines_int { ]] .. count .. '}' )
end
function piton.ComputeRange(marker_beginning,marker_end,file_name)
  local s = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( marker_beginning )
  local t = ( Cs ( ( P '##' / '#' + 1 ) ^ 0 ) ) : match ( marker_end )
  local first_line = -1
  local count = 0
  local last_found = false
  for line in io.lines ( file_name )
  do if first_line == -1
     then if string.sub ( line , 1 , #s ) == s
          then first_line = count
          end
     else if string.sub ( line , 1 , #t ) == t
          then last_found = true
               break
          end
     end
     count = count + 1
  end
  if first_line == -1
  then sprintL3 [[ \__piton_error_or_warning:n { begin~marker~not~found } ]]
  else if last_found == false
       then sprintL3 [[ \__piton_error_or_warning:n { end~marker~not~found } ]]
       end
  end
  sprintL3 (
      [[ \int_set:Nn \l__piton_first_line_int { ]] .. first_line .. ' + 2 }'
      .. [[ \int_set:Nn \l__piton_last_line_int { ]] .. count .. ' }' )
end
function piton.new_language ( lang , definition )
  lang = string.lower ( lang )
  local alpha , digit = lpeg.alpha , lpeg.digit
  local letter = alpha + S "@_$" -- $
  local other = S "+-*/<>!?:;.()@[]~^=#&\"\'\\$" -- $
  function add_to_letter ( c )
     if c ~= " " then letter = letter + c end
  end
  function add_to_digit ( c )
     if c ~= " " then digit = digit + c end
  end
  local strict_braces  =
    P { "E" ,
        E = ( "{" * V "F" * "}" + ( 1 - S ",{}" ) ) ^ 0  ,
        F = ( "{" * V "F" * "}" + ( 1 - S "{}" ) ) ^ 0
      }
  local cut_definition =
    P { "E" ,
        E = Ct ( V "F" * ( "," * V "F" ) ^ 0 ) ,
        F = Ct ( space ^ 0 * C ( alpha ^ 1 ) * space ^ 0
                * ( "=" * space ^ 0 * C ( strict_braces ) ) ^ -1 )
      }
  local def_table = cut_definition : match ( definition )
  local tex_braced_arg = "{" * C ( ( 1 - P "}" ) ^ 0 ) * "}"
  local tex_arg = tex_braced_arg + C ( 1 )
  local tex_option_arg =  "[" * C ( ( 1 - P "]" ) ^ 0 ) * "]" + Cc ( nil )
  local args_for_morekeywords
    = "[" * C ( ( 1 - P "]" ) ^ 0 ) * "]"
       * space ^ 0
       * tex_option_arg
       * space ^ 0
       * tex_arg
       * space ^ 0
       * ( tex_braced_arg + Cc ( nil ) )
  local args_for_moredelims
    = ( C ( P "*" ^ -2 ) + Cc ( nil ) ) * space ^ 0
      * args_for_morekeywords
  local args_for_morecomment
    = "[" * C ( ( 1 - P "]" ) ^ 0 ) * "]"
       * space ^ 0
       * tex_option_arg
       * space ^ 0
       * C ( P ( 1 ) ^ 0 * -1 )
  local args_for_tag
    = ( P "*" ^ -2 )
       * space ^ 0
       * ( "[" * ( 1 - P "]" ) ^ 0 * "]" ) ^ 0
       * space ^ 0
       * tex_arg
       * space ^ 0
       * tex_arg
  local sensitive = true
  local left_tag , right_tag
  for _ , x in ipairs ( def_table ) do
    if x[1] == "sensitive" then
      if x[2] == nil or ( P "true" ) : match ( x[2] ) then
        sensitive = true
      else
        if ( P "false" + P "f" ) : match ( x[2] ) then sensitive = false end
      end
    end
    if x[1] == "alsodigit" then x[2] : gsub ( "." , add_to_digit ) end
    if x[1] == "alsoletter" then x[2] : gsub ( "." , add_to_letter ) end
    if x[1] == "tag" then
      left_tag , right_tag = args_for_tag : match ( x[2] )
    end
  end
  local Number =
    K ( 'Number' ,
        ( digit ^ 1 * "." * # ( 1 - P "." ) * digit ^ 0
          + digit ^ 0 * "." * digit ^ 1
          + digit ^ 1 )
        * ( S "eE" * S "+-" ^ -1 * digit ^ 1 ) ^ -1
        + digit ^ 1
      )
  local alphanum = letter + digit
  local identifier = letter * alphanum ^ 0
  local Identifier = K ( 'Identifier' , identifier )
  local split_clist =
    P { "E" ,
         E = ( "[" * ( 1 - P "]" ) ^ 0 * "]" ) ^ -1
             * ( P "{" ) ^ 1
             * Ct ( V "F" * ( "," * V "F" ) ^ 0 )
             * ( P "}" ) ^ 1 * space ^ 0 ,
         F = space ^ 0 * C ( letter * alphanum ^ 0 + other ^ 1 ) * space ^ 0
      }
  local function keyword_to_lpeg ( name )
  return
    Q ( Cmt (
              C ( identifier ) ,
              function(s,i,a) return string.upper(a) == string.upper(name) end
            )
      )
  end
  local Keyword = P ( false )
  for _ , x in ipairs ( def_table )
  do if x[1] == "morekeywords"
        or x[1] == "otherkeywords"
        or x[1] == "moredirectives"
        or x[1] == "moretexcs"
     then
        local keywords = P ( false )
        local style = [[ \PitonStyle{Keyword} ]]
        if x[1] == "moredirectives" then style = [[ \PitonStyle{directive} ]] end
        style =  tex_option_arg : match ( x[2] ) or style
        local n = tonumber (style )
        if n then
         if n > 1 then style = [[ \PitonStyle{Keyword ]] .. style .. "}" end
        end
        for _ , word in ipairs ( split_clist : match ( x[2] ) ) do
          if x[1] == "moretexcs" then
            keywords = Q ( [[ \ ]] .. word ) + keywords
          else
            if sensitive
            then keywords = Q ( word  ) + keywords
            else keywords = keyword_to_lpeg ( word ) + keywords
            end
          end
        end
        Keyword = Keyword +
           Lc ( "{" .. style .. "{" ) * keywords * Lc "}}"
     end
     if x[1] == "keywordsprefix" then
       local prefix = ( ( C ( 1 - P " " ) ^ 1 ) * P " " ^ 0 ) : match ( x[2] )
       Keyword = Keyword + K ( 'Keyword' , P ( prefix ) * alphanum ^ 0 )
     end
  end
  local long_string  = P ( false )
  local LongString = P (false )
  local central_pattern = P ( false )
  for _ , x in ipairs ( def_table ) do
    if x[1] == "morestring" then
      arg1 , arg2 , arg3 , arg4 = args_for_morekeywords : match ( x[2] )
      arg2 = arg2 or [[ \PitonStyle{String.Long} ]]
      if arg1 == "s" then
        long_string =
          Q ( arg3 )
          * ( Q ( ( 1 - P ( arg4 ) - S "$\r" ) ^ 1 ) -- $
              + EOL
            ) ^ 0
          * Q ( arg4 )
      else
        central_pattern = 1 - S ( " \r" .. arg3 )
        if arg1 : match "b" then
          central_pattern = P ( [[ \ ]] .. arg3 )  + central_pattern
        end
        if arg1 : match "d" or arg1 == "m" then
          central_pattern = P ( arg3 .. arg3 )  + central_pattern
        end
        if arg1 == "m"
        then prefix = P ( false )
        else prefix = lpeg.B ( 1 - letter - ")" - "]" )
        end
        long_string = long_string +
           prefix
           * Q ( arg3 )
           * ( VisualSpace + Q ( central_pattern ^ 1 ) + EOL ) ^ 0
           * Q ( arg3 )
      end
      LongString = LongString +
         Ct ( Cc "Open" * Cc ( "{" ..  arg2 .. "{" ) * Cc "}}" )
         * long_string
         * Ct ( Cc "Close" )
    end
  end

  local braces = Compute_braces ( String )
  if piton.beamer then Beamer = Compute_Beamer ( lang , braces ) end

  DetectedCommands = Compute_DetectedCommands ( lang , braces )

  LPEG_cleaner[lang] = Compute_LPEG_cleaner ( lang , braces )
  local CommentDelim = P ( false )

  for _ , x in ipairs ( def_table ) do
    if x[1] == "morecomment" then
      local arg1 , arg2 , other_args = args_for_morecomment : match ( x[2] )
      arg2 = arg2 or [[ \PitonStyle{Comment} ]]
      if arg1 : match "i" then arg2 = [[ \PitonStyle{Discard} ]] end
      if arg1 : match "l" then
        local arg3 = ( tex_braced_arg + C ( P ( 1 ) ^ 0 * -1 ) )
                     : match ( other_args )
        if arg3 == [[\#]] then arg3 = "#" end -- mandatory
        CommentDelim = CommentDelim +
            Ct ( Cc "Open"
                 * Cc ( "{" .. arg2 .. "{" ) * Cc "}}" )
                 *  Q ( arg3 )
                 * ( CommentMath + Q ( ( 1 - S "$\r" ) ^ 1 ) ) ^ 0 -- $
            * Ct ( Cc "Close" )
            * ( EOL + -1 )
      else
        local arg3 , arg4 =
          ( tex_arg * space ^ 0 * tex_arg ) : match ( other_args )
        if arg1 : match "s" then
          CommentDelim = CommentDelim +
              Ct ( Cc "Open" * Cc ( "{" .. arg2 .. "{" ) * Cc "}}" )
              * Q ( arg3 )
              * (
                  CommentMath
                  + Q ( ( 1 - P ( arg4 ) - S "$\r" ) ^ 1 ) -- $
                  + EOL
                ) ^ 0
              * Q ( arg4 )
              * Ct ( Cc "Close" )
        end
        if arg1 : match "n" then
          CommentDelim = CommentDelim +
            Ct ( Cc "Open" * Cc ( "{" .. arg2 .. "{" ) * Cc "}}" )
             * P { "A" ,
                  A = Q ( arg3 )
                      * ( V "A"
                          + Q ( ( 1 - P ( arg3 ) - P ( arg4 )
                                  - S "\r$\"" ) ^ 1 ) -- $
                          + long_string
                          +   "$" -- $
                              * K ( 'Comment.Math' , ( 1 - S "$\r" ) ^ 1 ) --$
                              * "$" -- $
                          + EOL
                        ) ^ 0
                      * Q ( arg4 )
                 }
            * Ct ( Cc "Close" )
        end
      end
    end
    if x[1] == "moredelim" then
      local arg1 , arg2 , arg3 , arg4 , arg5
        = args_for_moredelims : match ( x[2] )
      local MyFun = Q
      if arg1 == "*" or arg1 == "**" then
        MyFun = function ( x ) return K ( 'ParseAgain.noCR' , x ) end
      end
      local left_delim
      if arg2 : match "i" then
        left_delim = P ( arg4 )
      else
        left_delim = Q ( arg4 )
      end
      if arg2 : match "l" then
        CommentDelim = CommentDelim +
            Ct ( Cc "Open" * Cc ( "{" .. arg3 .. "{" ) * Cc "}}" )
            * left_delim
            * ( MyFun ( ( 1 - P "\r" ) ^ 1 ) ) ^ 0
            * Ct ( Cc "Close" )
            * ( EOL + -1 )
      end
      if arg2 : match "s" then
        local right_delim
        if arg2 : match "i" then
          right_delim = P ( arg5 )
        else
          right_delim = Q ( arg5 )
        end
        CommentDelim = CommentDelim +
            Ct ( Cc "Open" * Cc ( "{" .. arg3 .. "{" ) * Cc "}}" )
            * left_delim
            * ( MyFun ( ( 1 - P ( arg5 ) - "\r" ) ^ 1 ) + EOL ) ^ 0
            * right_delim
            * Ct ( Cc "Close" )
      end
    end
  end

  local Delim = Q ( S "{[()]}" )
  local Punct = Q ( S "=,:;!\\'\"" )
  local Main =
         space ^ 1 * -1
       + space ^ 0 * EOL
       + Space
       + Tab
       + Escape + EscapeMath
       + CommentLaTeX
       + Beamer
       + DetectedCommands
       + CommentDelim
       + Delim
       + LongString
       + Keyword * ( Space + Punct + Delim + EOL + -1 )
       + Punct
       + K ( 'Identifier' , letter * alphanum ^ 0 )
       + Number
       + Word
  LPEG1[lang] = Main ^ 0
  if left_tag then
  end
  LPEG2[lang] =
    Ct (
         ( space ^ 0 * P "\r" ) ^ -1
         * BeamerBeginEnvironments
         * Lc [[ \__piton_begin_line: ]]
         * SpaceIndentation ^ 0
         * LPEG1[lang]
         * -1
         * Lc [[ \__piton_end_line: ]]
       )
  if left_tag then
    local Tag = Q ( left_tag * other ^ 0 )
                * ( ( ( 1 - P ( right_tag ) ) ^ 0 )
                  / ( function ( x ) return LPEG0[lang] : match ( x ) end ) )
                * Q ( right_tag )
    MainWithoutTag
            = space ^ 1 * -1
            + space ^ 0 * EOL
            + Space
            + Tab
            + Escape + EscapeMath
            + CommentLaTeX
            + Beamer
            + DetectedCommands
            + CommentDelim
            + Delim
            + LongString
            + Keyword * ( Space + Punct + Delim + EOL + -1 )
            + Punct
            + K ( 'Identifier' , letter * alphanum ^ 0 )
            + Number
            + Word
    LPEG0[lang] = MainWithoutTag ^ 0
    MainWithTag
            = space ^ 1 * -1
            + space ^ 0 * EOL
            + Space
            + Tab
            + Escape + EscapeMath
            + CommentLaTeX
            + Beamer
            + DetectedCommands
            + CommentDelim
            + Tag
            + Delim
            + Punct
            + K ( 'Identifier' , letter * alphanum ^ 0 )
            + Word
    LPEG1[lang] = MainWithTag ^ 0
    LPEG2[lang] =
      Ct (
           ( space ^ 0 * P "\r" ) ^ -1
           * BeamerBeginEnvironments
           * Lc [[ \__piton_begin_line: ]]
           * SpaceIndentation ^ 0
           * LPEG1[lang]
           * -1
           * Lc [[ \__piton_end_line: ]]
         )
  end
end

