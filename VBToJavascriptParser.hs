module VBToJavascriptParser (
    parser
) where

import Replace
import qualified Parser

replaceRules = [ReplaceExact "<DataContract()>" "",
                ReplaceExact "<DataMember()>" "",
                ReplaceExact "<EnumMember()>" "",
                ReplaceExact "As New" "= new",
                ReplaceAllAfterUntil " As" "" " =",
                ReplaceAllAfterUntil " As" "" ",",
                ReplaceAllAfterUntil " As" "" ") ",
                ReplaceAllAfterUntil " As" "" " In",
                ReplaceExact " In " " in ",
                ReplaceAllAfter " As" "",
                ReplaceExactAndAppend "Public Class" "class" " {",
                ReplaceExact "Public Property" "",
                ReplaceExact "End Class" "}",
                ReplaceExactAndAppend "Public Enum" "const" " {",
                ReplaceExactAndAppend "Function " "" " {",
                ReplaceExactAndAppend "Sub " "" " {",
                ReplaceExact "End Enum" "}",
                ReplaceExact "End Sub" "}",
                ReplaceExact "End Function" "}",
                ReplaceExact "Private Shared" "static",
                ReplaceExact "Public Shared" "static",
                ReplaceExact " OrElse" " || ",
                ReplaceExact " AndAlso" " && ",
                ReplaceExact " Or" " || ",
                ReplaceExact " And" " && ",
                ReplaceExact "IsNot" "!=",
                ReplaceExact "Not " "!",
                ReplaceExact "End If" "}",
                ReplaceExact "If " "if (",
                ReplaceExact " Then" ") {",
                ReplaceExact "Else If" "} else if (",
                ReplaceExact "Else" "} else {",
                ReplaceExact "Dim " "var ",
                ReplaceExact "'" "//",
                ReplaceExact "ByVal " "",
                ReplaceExact "ByRef " "",
                ReplaceExactAndAppend "For Each" "for (var" ") {",
                ReplaceExact "Next" "}",
                ReplaceExact "<>" "!==",
                ReplaceExact "Nothing " "undefined",
                ReplaceExact "Return " "return "]

parser = Parser.Parser { Parser.id = "vbToJavascript", Parser.name = "VB to Javascript", Parser.rules = replaceRules, Parser.fileExt = ".js"}