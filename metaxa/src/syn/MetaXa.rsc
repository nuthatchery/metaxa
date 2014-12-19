module syn::MetaXa

start syntax Module = Mod: Decl*;

syntax Decl
	= SortDecl: "sort" Id "{" Decl* "}"
	| ConstructDecl: "construct" Id "(" {ParamDecl ","}* ")" "{" Def* "}"
	| Nod: ";"
	;

syntax Def
	= SyntaxDef: SyntaxModifier* "syntax" SyntaxBody
	| SugarDef: SyntaxModifier* "syntax" SyntaxBody SugarOp SyntaxBody
	| Nod: ";"
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
	| Star: SortExpr "*"
	| Plus: SortExpr "+"
	| Sep: "{" SortExpr SortExpr "}"
	| bracket "(" SortExpr ")"
	| Alt: SortExpr "|" SortExpr
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

anno loc Tree@\loc;
