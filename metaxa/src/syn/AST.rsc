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
	| Nod()
	;
	
public data ASTDef =
	SyntaxDef(list[ASTSyntaxModifier] mods, ASTSyntaxBody sb)
	// TODO SugarDef
	;

public data ASTParamDecl 
            // TODO TypeExpr ?
	= Param(TypeExpr te, str id);
	
public data ASTSyntaxModifier
	= Sugar()
	| Deprecated()
	;
	
public data ASTSyntaxBody
	= SyntaxTokens(list[ASTSyntaxToken] sts)
	;

public data ASTSyntaxToken 
	= Literal(str l)
	// TODO the rest of 'em
	;