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



