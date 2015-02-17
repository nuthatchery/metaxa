/*
Use `implode(#AST_, p)` - where `p` is a parse tree, and `AST_` is the AST type - to get the corresponding AST.
*/
module syn::AST

import syn::MetaXa;
import syn::AST;
import ParseTree;

// AST types are suffixed with 'AST' to be sure that name-clashing with the syntax productions is avoided.
public data ASTModule
	= Mod(list[ASTDecl])
	;
	
public data ASTDecl
	= SortDecl(str id, list[ASTDecl] decls)
	| ConstructDecl(str id, list[ASTParamDecl] pds, list[ASTDef] defs)
	| Lex(ASTLex lex)
	| Nod()
	;

public data ASTDef
	= SyntaxDef(list[ASTSyntaxModifier] mods, ASTSyntaxBody sb)
	// TODO SugarDef
	;

public data ASTLex
	= L(str id, list[ASTSortExpr] ses) // NOTE can also be represented as `str`. Then you don't have to jump through any unnecessary AST hoops and can just translate directly to rascal source form.
	;

public data ASTParamDecl 
	= Param(ASTTypeExpr te, str id)
	;

// TODO finish	
public data ASTTypeExpr 
	= Type(str id)
	| Parameterized(str id, list[ASTTypeExpr] te)
	;

	
public data ASTSyntaxModifier
	= Sugar()
	| Deprecated()
	;
	
public data ASTSyntaxBody
	= SyntaxTokens(list[ASTSyntaxToken] sts)
	;

public data ASTSyntaxToken 
	= Literal(str l)
	| TypeOrVar(ASTSortExpr se)
	;
	
public data ASTSortExpr 
	= Sort(str id)
	| Star(ASTSortExpr se)
	| Plus(ASTSortExpr se)
	| CharacterClass(ASTClass c)
	| Follow(ASTSortExpr se1, ASTSortExpr se2)
	| NotFollow(ASTSortExpr se1, ASTSortExpr se2)
	| Precede(ASTSortExpr se1, ASTSortExpr se2)
	| NotPrecede(ASTSortExpr se1, ASTSortExpr se2)
	// TODO implement the rest
	;

public data ASTClass
	= SimpleCharclass(list[ASTRange] ranges)
	;

public data ASTRange
	= FromTo(str from, str to)
	| Character(str c)
	;
