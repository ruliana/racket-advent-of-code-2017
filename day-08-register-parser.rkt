#lang brag
; From lexer (tokens) to syntax (AST)
; Exposes a "parse" function
program : [line] (/NEWLINE [line])*
line : register command amount if register comparator amount
     | register command amount
@register : IDENTIFIER
@command : IDENTIFIER
@amount : NUMBER
@if : IDENTIFIER
@comparator : IDENTIFIER
