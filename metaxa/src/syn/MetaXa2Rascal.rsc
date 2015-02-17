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

import String;
import List;
import IO;

/********

RASCAL SYNTAX DESCRIPTION GENERATION

We leverage the functions from the AST GRAMMAR section to generate a (textual) rascal syntax definition module.

********/

public str metaxa2rascal( ASTModule m ) {
	gd = grammarDefClean( m );
	return definition2rascal( gd );
}

/*
Convert a ASTModule to a GrammarDefinition, and clean up this representation.

"Clean up" in the sense that the data type is simplified; certain patterns in the data structure is converted to a
simpler, equivalent pattern.

This may make further processing of the data type more efficient, for example converting the GrammarDefinition to a textual rascal module.
It might also avoid certain patterns in the data structure that might lead to an invalid (won't be parsed) textual rascal module,
as we have experienced before.
*/
// NOTE maybe the grammarDefinition(...) function should take care to not generate troublesome nodes (such as \alt with only one alternative)
// to begin with, instead of processing them out in this function? This function could also be used as a test to assert that the
// grammarDefinition(...) function upholds this responsibility.
GrammarDefinition grammarDefClean( ASTModule m ) {
	gd = grammarDefinition(m);
	gd = visit(gd) {
		case \alt({x}) => x
		case \seq([x]) => x
		case \prod(s, [seq([])], as) => \prod(s, [], as)
	}
	return gd;
}

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
	set[str] imports = {}; // TODO fix/change?
	exten = {}; // TODO fix/change?
	gram = declsToGrammar(m);
	return \module(name, imports, exten, gram);
}

/*
Return the metaxa description as a Grammar
*/
Grammar declsToGrammar( ASTModule m: Mod(decls) ) {
	st = starts(m);
	map[Symbol, Production] rules = concatMaps( mapper(decls, declToProdTuple) );
	return \grammar(st, rules);
}

// TODO generalize types of the map?
// TODO put precondition that no two maps in the list contain the same key?
map[Symbol, Production] concatMaps(list[ map[Symbol, Production] ] l)
	  // This expression is a Reducer
	= ( () | it + e | map[Symbol, Production] e <- l);

/*
The symbols representing the start productions.
*/
set[Symbol] starts( ASTModule _: Mod(decls) ) {
	m = mapper(decls, id);
	return toSet( m );
}

/*
Return the Production of a declaration, and its name.

Convenient for helping in making a Grammar, since one of its parameters is a Map with
Symbol (name) as the keys and Production as the values.
*/
map[Symbol, Production] declToProdTuple( ASTDecl c: ConstructDecl(_, _, _) ) {
	switch (constructToProd(c)) {
	case p:\prod(name, symbols, attr):
		return (name : p);
	}
}

map[Symbol, Production] declToProdTuple( ASTDecl c: Lex(_) ) {
	switch (lexToProd(c)) {
	case p:\prod(name, symbols, _):
		return (name : p);
	}
}

map[Symbol, Production] declToProdTuple( ASTDecl c: SortDecl(str id, list[ASTDecl] decls) ) {
	// TODO assumes that `decls` contains no `sort` declarations: needs to be fixed.
	// TODO does not "preserve" the scope of the declarations: declarations with the same name declared in different scopes *will* collide.
	productions = mapper(decls, declToProdTuple);
	return concatMaps(productions);
}

// TODO instead make this function return a Symbol?
// constructs with no parent (no `sort`) should also return a production, but that is a special case.
// TODO make a function that takes a construct and returns a Production, namely the \prod(...) constructor
Production constructToProd( ASTDecl c: ConstructDecl(_, _, _) ) {
	name = id(c);
	symbols = [construct(c)];
	return \prod(name, symbols, {});
}

Production lexToProd( ASTDecl c: Lex(L(i, ses)) ) {
	name = \lex(i);
	symbols = symbol(ses);
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
	return \alt( toSet(all_defs) );
}

Symbol def( SyntaxDef(mods, sb) )
	= symbol(sb);

Symbol symbol( SyntaxTokens(sts) )
	= \seq( symbol(sts) ); // see: http://tutor.rascal-mpl.org/Rascal/Rascal.html#/Rascal/Libraries/Prelude/ParseTree/Symbol/Symbol.html

/*
Get the list of symbols in a SyntaxBody

Example:

syntax { first <prod> then }

Should return three symbols; one representing the literal "first", then a symbol representing the production prod,
and then then the symbol representing the literal "then".
*/
list[Symbol] symbol( list[ASTSyntaxToken] sts )
	= mapper( sts, syntaxToken );
	
list[Symbol] symbol( list[ASTSortExpr] ses )
	= mapper( ses, sortExpr );	

/*
Convert a syntax-token to something which can be used in the further processing to a Rascal syntax description AST (data type Grammar).
*/
Symbol syntaxToken( Literal(s) )
	= \lit(s);

// TODO finish 
Symbol syntaxToken( TypeOrVar(se) )
	= sortExpr(se);

Symbol sortExpr( Sort(s) )
	= sort(s);
Symbol sortExpr( Star(se) )
	= \iter-star( sortExpr(se) );
Symbol sortExpr( Plus(se) )
	= \iter( sortExpr(se) );
Symbol sortExpr( CharacterClass(SimpleCharclass(rs)) ) {
	l = mapper(rs, charRange);
	return \char-class( mapper(rs, charRange) );
}

CharRange charRange( FromTo(f, t) )
	         // we need the code point (ASCII) of the character.
	         // charAt(...) returns the codepoint of the character at the given index.
	= range( charAt(f, 0), charAt(t, 0) );

CharRange charRange( Character(c) ) {
	// TODO is this correct?
	codepoint = charAt(c, 0); 
	return range(codepoint, codepoint);
}

/*
get the id of a declaration.
*/
Symbol id( SortDecl(str i, list[ASTDecl] _) ) {
	return sort(i);
}

Symbol id( ConstructDecl(str i, list[ASTParamDecl] _, list[ASTDef] _) ) {
	return sort(i);
}

Symbol id( Lex(L(i, _)) ) {
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

/*****

UNIT TESTS

These tests may be used for work-in-progress stuff. That is, tests that don't necessarily work but should once some
things have been implemented, and/or bugs have been fixed.

*****/

// ...

/*****

REGRESSSION TESTS

These tests should work in any new commits; if they don't then a new bug might have been introduced.

*****/

/*
We should be able to parse the empty.mtx file and convert it to a rascal syntax module.
This test does not test that the output itself is correct.
*/
test bool canConvertEmpty() {
	p = parse(#Module, |file:///home/kristoffer/git/metaxa/metaxa/src/examples/empty.mtx|);
	i = implode(#ASTModule, p);
	metaxa2rascal(i);
	return true;
}

test bool canConvertHelloworld() {
	p = parse(#Module, |file:///home/kristoffer/git/metaxa/metaxa/src/examples/helloworld.mtx|);
	i = implode(#ASTModule, p);
	metaxa2rascal(i);
	return true;
}

test bool canConvertConstantAlternatives() {
	p = parse(#Module, |file:///home/kristoffer/git/metaxa/metaxa/src/examples/constant_alternatives.mtx|);
	i = implode(#ASTModule, p);
	metaxa2rascal(i);
	return true;
}