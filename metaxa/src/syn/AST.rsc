/*
Use `implode(#AST, p)` - where `p` is a parse tree - to get the corresponding AST.
*/
module syn::AST

import syn::MetaXa;
import syn::AST;
import ParseTree;

data AST 
	= Mod(list[Decl])
	;
	
data Decl
	= SortDecl(str id, list[Decl] decls)
	| ConstructDecl(str id, list[ParamDecl] pds, list[Def] defs)
	| Nod()
	;
	
data Def =
	SyntaxDef(list[SyntaxModifier] mods, SyntaxBody sb)
	// TODO SugarDef
	;

data ParamDecl 
	= Param(TypeExpr te, str id);
	
data SyntaxModifier
	= Sugar()
	| Deprecated()
	;
	
data SyntaxBody
	= SyntaxTokens(list[SyntaxToken] sts)
	;

data SyntaxToken 
	= Literal(str l)
	// TODO the rest of 'em
	;