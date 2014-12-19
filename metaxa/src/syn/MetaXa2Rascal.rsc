/**
Generate a rascal grammar from a metaxa description.
*/
module syn::MetaXa2Rascal

import syn::MetaXa;
import syn::AST;

import ParseTree;
// AST for rascal grammar
import Grammar;
// Convert AST rascal grammar to rascal syntax definition (source code)
import lang::rascal::format::Grammar;


import List;
import IO;

/********

RASCAL SYNTAX DESCRIPTION GENERATION

We leverage the functions from the AST GRAMMAR section to generate a (textual) rascal syntax definition module.

********/

public str metaxa2rascal( ASTModule m )
	= definition2rascal( grammarDefinition(m) );

/********

AST GRAMMAR

*********/

/*
A scope of declarations
*/
alias Scope = map[str, str];

GrammarDefinition grammarDefinition( ASTModule m ) {
	main = "unknown_main"; // TODO fix?
	modules = ( "the-module" : grammarModule(m) ); // TODO singleton map, and hard-coded key. Change?
	return \definition(main, modules);
}

GrammarModule grammarModule( ASTModule m ) {
	name = "unknown"; // TODO fix?
	set[str] imports = {}; // TODO fix?
	// TODO weird error message for the following, due to "type inference". 
	// Easy to fix (put in type declaration, if I recall correctly), but I'm curious as to why I get the error to begin with.
	exten = {}; // TODO fix?
	gram = declsToGrammar(m);
	return \module(name, imports, exten, gram);
}

/*
Return the metaxa description as a Grammar
*/
Grammar declsToGrammar( ASTModule m: Mod(decls) ) {
	st = starts(m);
	// note: important to use `toMapUnique` rather than `toMap`, here
	map[Symbol, Production] rules = toMapUnique( mapper(decls, declToProdTuple) );
	return \grammar(st, rules);
}

/*
The symbols representing the start productions.
*/
set[Symbol] starts( ASTModule \mod ) {
	switch (\mod) {
		
	case Mod(decls): {
		// TODO bug here
		// The error will be, for some reason, reported in the List module, namely at the line of the `mapper` implementation.
		//m = mapper(decls, id);
		
		// Alternative to the above.
		// we write out the definition/implementation of `mapper` (copy pasted from the List module),
		// so that we get an error message in this module rather than in the List module. 
		list[Symbol] m2 = [id(d) | ASTDecl d <- decls];
		
		return toSet( m2 );
	}
	}
}

/*
Return the Production of a declaration, and its name.

Convenient for helping in making a Grammar, since one of its parameters is a Map with
Symbol (name) as the keys and Production as the values.
*/
tuple[Symbol, Production] declToProdTuple( ASTDecl c ) {
	switch (constructToProd(c)) {
	case p:\prod(name, symbols, attr):
		return <name, p>;
	}
}

// TODO instead make this function return a Symbol?
// constructs with no parent (no `sort`) should also return a production, but that is a special case.
// TODO make a function that takes a construct and returns a Production, namely the \prod(...) constructor
Production constructToProd( ASTDecl c: ConstructDecl(_, _, _) ) {
	name = id(c);
	symbols = [construct(c)];
	return \prod(name, symbols, {});
}

/*
Returns a construct as a Symbol which is a set of alternatives.

More concretely, we assume that syntax definitions in the construct is on this form:

construct(...) {
	syntax {...}
	syntax {...}
	syntax {...}
	...
}

And we make this into this \alt, more or less:

alt(...) = syntax {...} | syntax {...} | syntax {...} | ... 
*/
Symbol construct( ConstructDecl(id, pds, defs) ) {
	all_defs = mapper(defs, def);
	return \alt( toSet(defs) );
}

// TODO consider the syntax modifiers?
Symbol def( SyntaxDef(mods, SyntaxBody sb) ) 
	= symbol(sb);

Symbol symbol( SyntaxBody sb )
	= \seq( symbol(sb) ); // see: http://tutor.rascal-mpl.org/Rascal/Rascal.html#/Rascal/Libraries/Prelude/ParseTree/Symbol/Symbol.html

/*
Get the list of symbols in a SyntaxBody

Example:

syntax { first <prod> then }

Should return three symbols; one representing the literal "first", then a symbol representing the production prod,
and then then the symbol representing the literal "then".
*/
// TODO error: "Undeclared type: Product". But why?
list[Symbol] symbol( SyntaxTokens(sts) )
	= mapper( sts, syntaxToken );

/*
Convert a syntax-token to something which can be used in the further processing to a Rascal syntax description AST (data type Grammar).
*/
Symbol syntaxToken( Literal(s) )
	= \lit(s);

// TODO the rest of the syntax token alternatives

/*
get the id of a declaration.
*/
Symbol id( SortDecl(str i, list[ASTDecl] _) ) {
	return sort(i);
}

Symbol id( ConstructDecl(str i, list[ASTParamDecl] _, list[ASTDef] _) ) {
	return sort(i);
}

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
