 @license{
  This program and the accompanying materials
  are made available under the terms of the Eclipse Public License v1.0:
  http://www.eclipse.org/legal/epl-v10.html
}
module syn::MetaXa

start syntax Module = Mod: Decl*;

syntax Decl
	= SortDecl: "sort" Id "{" Decl* "}"
	| ConstructDecl: "construct" Id "(" {ParamDecl ","}* ")" "{" Def* "}"
	| Lex: Lex
	| Nod: ";"
	;

syntax Def
	= SyntaxDef: SyntaxModifier* "syntax" SyntaxBody
	| SugarDef: SyntaxModifier* "syntax" SyntaxBody SugarOp SyntaxBody
	| Nod: ";"
	;

syntax Lex
	                      // We might also allow NonSpace in `lexical` declarations,
	                      // but that doesn't seem to be useful.
	= L: "lexical" Id "=" SortExpr* ";"
	;

syntax ParamDecl
	= Param: TypeExpr Id
	;

syntax SyntaxModifier
	= Deprecated: "deprecated"
	| Sugar: "sugar"
	;

syntax SyntaxBody
	= SyntaxTokens: "{" SyntaxToken* "}"
	;

syntax SyntaxToken
	= Literal: NonSpace
	| TypeOrVar: "\<" SortExpr "\>"
	| TypedVar: "\<" SortExpr Id "\>"
	;

syntax SortBody
	= SortBody: "{" Decl* "}"
	;

syntax SortExpr
	= Sort: Id
	| Parameterized: Id "[" {SortExpr ","}* "]"
	| CharacterClass: Class
	| Star: SortExpr "*"
	| Plus: SortExpr "+"
	| Sep: "{" SortExpr SortExpr "}"
	| bracket "(" SortExpr ")"
	| Alt: SortExpr "|" SortExpr
	// The following is based on a similar definition in Rascal.rsc
	> assoc (
	left ( Follow: SortExpr "\>\>" SortExpr
	     | NotFollow: SortExpr "!\>\>" SortExpr
	     )
	|
	right ( Precede: SortExpr "\<\<" SortExpr
	      | NotPrecede: SortExpr "!\<\<" SortExpr
	      )
	)
	;

syntax TypeExpr
	= Type: Id
	| Parameterized: Id "[" {TypeExpr ","}* "]"
	;

syntax Id
	= Name: LexId
	;

syntax SugarOp
	= Equiv: "\<-\>"
	| Sugar: "-\>"
	| Desugar: "\<-"
	;

lexical NonSpace = NonSpace: NonSpaceChar+ >> [\ \t\n\r\f\<\>{}]
	;

lexical NonSpaceChar
	= NonSpaceChar: ![\ \t\n\r\f\<\>\\{}]
	| Escaped: [\\]![];

lexical Escaped = [\\]![];

lexical LexId = [a-zA-Z_] !<< [a-zA-Z_] [a-zA-Z_0-9]* !>> [a-zA-Z_0-9];

// numbers
lexical LexNum = [0-9] !<< [0-9]+ !>> [0-9];

layout Layout = Layout: (Space | Comment)* !>> [\ \t\n\r\f] !>> "//";

lexical Comment
	= @category="Comment" "//" ![\n\r]* $
	;

lexical Space = [\ \t\n\r\f];

/*
The following has been taken/adapted from the Rascal syntax definition:
https://github.com/cwi-swat/rascal/blob/master/src/org/rascalmpl/library/lang/rascal/syntax/Rascal.rsc#L822
We want a similar syntax for defining character classes and other lexical syntax as Rascal.
So adapting their syntax might make more sense than doing it from scratch ourselves.
*/

syntax Class
	= SimpleCharclass: "[" Range* "]"
	| Complement: "!" Class
	> left Difference: Class "-" Class
	> left Intersection: Class "&&" Class
	> left Union: Class "||" Class
	| bracket \bracket: "(" Class ")"
	;

syntax Range
	= FromTo: Char "-" Char
	| Character: Char
	;

lexical Char
	= @category="Constant" "\\" [\ \" \' \- \< \> \[ \\ \] b f n r t]
	| @category="Constant" ![\ \" \' \- \< \> \[ \\ \]]
// The following may not be needed, so it is commented out (for now)
//	| @category="Constant" UnicodeEscape
	;

lexical UnicodeEscape
	= utf16: "\\" [u] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f]
	| utf32: "\\" [U] (("0" [0-9 A-F a-f]) | "10") [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] // 24 bits
	| ascii: "\\" [a] [0-7] [0-9A-Fa-f]
	;

/*
END of definitions taken and adapted from Rascal.rsc
*/

anno loc Tree@\loc;
