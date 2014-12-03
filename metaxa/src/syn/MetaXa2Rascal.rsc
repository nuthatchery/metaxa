/**
Generate a rascal grammar from a metaxa description.
*/
module syn::MetaXa2Rascal

import syn::MetaXa;

// AST for rascal grammar
import Grammar;
// Convert AST rascal grammar to rascal syntax definition (source code)
import lang::rascal::format::Grammar;

import List;
import IO;

/*
A scope of declarations
*/
alias Scope = map[str, str];

/*
The different scopes in a module.

Each member of the list should correspond to the scope of a "sort", where each sort is a start syntax production.
*/
// at least useful for testing scopeOfDecl at the console, by parsing and passing in one of the example grammar definitions
list[Scope] allScopes( (Module)`<Decl* decls>` ) {
	sc = [];
	for ( d <- decls ) {
		sc += scopeOfDecl(d);
	}
	return sc;
}

/*
Scope of a declaration. 
*/
// TODO: make a function that returns scope for individiual constructs.
// The scopeOfDecl function will return the scope of a whole decl, which might span multiple constructs,
// and productions defined in one construct-scope should not be visible in other construct-scopes.
// TODO this function has almost the same structure as the previous "case function" in this module; use some higher-order function?
Scope scopeOfDecl( (Decl)`sort <Id id> { <Decl* ds> }` ) {
	sc = ();
	for ( p <- ds ) {
		sc += scopeOfDecl(p);
	}
	return sc;
}

Scope scopeOfDecl( (Decl)`construct <Id id> ( <{ParamDecl ","}* pds> ) { <Def* defs> }` ) {
	sc = ();
	for ( p <- pds ) {
		switch (p) {
		case (ParamDecl)`<TypeExpr te> <Id id>`: 
			sc += (unparse(id) : unparse(te));
		default:
			throw "should never happen";
		}
	}
	return sc;
}



