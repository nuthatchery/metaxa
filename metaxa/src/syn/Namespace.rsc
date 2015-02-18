// TODO could/should this module have a better name?
module syn::Namespace

import syn::AST;
import List;

// wrapper
public ASTDecl namespace( Mod(decls) ) 
	= mapper( decls, namespace );

/*
Recursively changes all identifiers to be prefixed with the name of the parent sort.

Example:

sort A {
	construct B ...
}

=>

sort A {
	construct A_B
}

The purpose is to preserve scoping of definitions in the translation to other formats, like rascal.
Since declarations can be nested in metaxa, there can be several declarations with the same name, but in different scopes.
Prefixing each declaration with its parent declaration's name preserves this scoping when translating to a format with
a flat namespace, like a rascal syntax definition.
*/
// TODO this implementation is not done, since it does not change the occurences of productions in definitions.
// So if, for example, a definition called "Id" is changed to something like "A_Id",
// any mention of that name still has the name "Id".
public ASTDecl namespace( ASTDecl _: SortDecl(i, decls) ) {
	newDecls = mapper( decls, 
	                          ASTDecl (ASTDecl d){ return prefix(i, d); } );
	return SortDecl( i, mapper( newDecls, namespace ));
}

// The other declarations (lexical, construct) are not nested, so they are not changed.
public default ASTDecl namespace( ASTDecl d )
	= d;
	
// 
ASTDecl pref( str prefix, ASTDecl d ) {
	switch (d) {
	case SortDecl(i, decls):            return SortDecl( fuse(pref,i), decls );
	case ConstructDecl(i, pds, defs):   return ConstructDecl( fuse(pref,i), pds, defs );
	case Lex( L(i, ses) ):              return Lex(L( fuse(pref,i), ses ));
	}
}

str fuse(str x, str y)
	= x + "_" + y;
