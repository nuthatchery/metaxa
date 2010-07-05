module XaLib
import IO;
import ParseTree;
import List;
import String;
import Set;
//import XaTree;

data RuntimeException = IllegalTreeFormat(str message, Tree tree)
	| UnboundVariable(str, list[str]);

/** Give the constructor of a tree.

	@param t	A tree
	@return		The constructor of t
*/
public str consOf(Tree t) {
	assert notAmb(t);
	if(prod(_,_,attrs(as)) := t.prod) {
		for(term(cons(c)) <- as) {
			return c;
		}
 	}

	throw IllegalTreeFormat("Failed to find constructor of tree", t);
}



/** Remove quotes from a tree.

	@param t	A quoted tree, e.g. `(: foo :)`
	@return		The tree, with quotes removed.
*/
public Tree unquote(Tree t) {
	assert notAmb(t);
	if(isCF(t))
	    for(i <- domain(t.args))
			if(isCF(t.prod.lhs[i])) {		
				if(baseSortName(sortOf(t)) in {"SQuoted", "DQuoted", "TQuoted", "Quoted"})
					return unquote(t.args[i]);
				else
					return t.args[i];
			}
			else if(\cf(iter(sort(_))) := t.prod.lhs[i]) {
				return t.args[i].args;
			}
	throw IllegalTreeFormat("Unquoted child node not found", t);

}


/** Remove quotes from a concrete syntax tree of a quoted list.

	@param t A tree with a quoted list, e.g., `(: <T* X> :)`
	@return The list, without quotes
*/
public list[Tree] unquoteList(Tree t) {
	assert notAmb(t);
	if(isCF(t))
	    for(i <- domain(t.args))
			if(\cf(iter(sort(_))) := t.prod.lhs[i]) {
				return t.args[i].args;
			}
	throw IllegalTreeFormat("Unquoted child node not found", t);

}

public bool isCF(Tree t) {
	return (\cf(sort(_)) := t.prod.rhs || \cf(\parameterized-sort(_,_)) := t.prod.rhs) ? false;
}

public bool isCF(Symbol s) {
	return (\cf(sort(_)) := s|| \cf(\parameterized-sort(_,_)) := s) ? false;
}

/** @param t A tree with list, e.g., `<T* X>`
	@return The list, as a Rascal list
*/
public list[Tree] asList(Tree t) {
	assert notAmb(t);
	if(\cf(\iter-star-sep(sort(srt),lit(sep))) := t.prod.rhs)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	else if(\cf(\iter-star(sort(srt),lit(sep))) := t.prod.rhs)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	else if(\cf(\iter-sep(sort(srt),lit(sep))) := t.prod.rhs)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	else if(\cf(\iter(sort(srt),lit(sep))) := t.prod.rhs)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	else if(\list(\cf(\iter(sort(srt)))) := t.prod)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	else if(\list(\cf(\iter-star(sort(srt)))) := t.prod)
		return [x | x <- t.args, \cf(sort(srt)) := x.prod.rhs];
	throw IllegalTreeFormat("List node not found", t);
}

/**	Find the SDF list  iter non-terminal of a tree.

	@param t	A tree, containing the relevant iteration symbol
	@param c	The constructor, to be turned into a sort
	@return		An SDF non-terminal, e.g. 'Stat*' or '{Expr ","}*'
*/
public str sdfListOf(Tree t, str c) {
	assert notAmb(t);
	str iterSym;
	if(/MXaREP r <- t.args)
		iterSym = r;
	else if(/lit(r) <- t.prod)
		iterSym = r;
	else
		throw IllegalTreeFormat("No repetition symbol found in list match variable", t);

	if(c in {"Identifier", "Var", "Expr"})
		return "{<c> \",\"}<iterSym>";
	else
		return "<c><iterSym>";
}

/** Return the nth argument of a tree. */
public Tree argOf(Tree t, int n) {
	assert notAmb(t);
	return t.args[n];
}

/** Return the nth argument of a tree, skipping concrete nodes. */
public Tree astArgOf(Tree t, int n) {
	assert notAmb(t);
	for(i <- [0 .. size(t.prod.lhs)])
		if(\cf(opt(\layout())) := t.prod.lhs[i])
			;
		else if(\cf(_) := t.prod.lhs[i]) {
			if(n == 0)
				return t.args[i];
			n = n - 1;
		}
	throw IllegalTreeFormat("No AST child <n> found", t);
}

/** Join a list of strings, with a separator */
public str strJoin(list[str] l, str sep) {
	if(size(l) == 0)
		return "";
	else
		return (head(l)|it+sep+x|x <- tail(l));
}


/** Parse a string as a MXaSort */
public Tree asSdfSort(str s) {
	if(/^[A-Za-z][A-Za-z0-9]*$/ := s)
		return parse(#MXaID, s);
	else
		return parse(#MXaSort, s);
}

/** Parse a string as a MXaID */
public Tree asMXaID(str s) {
	return parse(#MXaID, s);
}

public tuple[list[&T], list[&U]] unzip(list[tuple[&T,&U]] l) {
	list[&T] ts = [];
	list[&U] us = [];

	for(<x,y> <- l) {
		ts += x;
		us += y;
	}
	return <ts,us>;
}


public str varRename(str n) {
	return replaceAll(n, "\'", "_");
}

public Tree varRename(Tree n) {
	return visit(n) {
		case char(39) => char(95)
	};
}

public str varUnrename(str n) {
	return replaceAll(n, "_", "\'");
}


public str sortOfList(Symbol sym) {
	switch(sym) {
		case \cf(\iter-star-sep(s, _)):
			return sortName(s);
		case \cf(\iter-sep(s, _)):
			return sortName(s);
		case \cf(\iter-star(s)):
			return sortName(s);
		case \cf(\iter(s)):
			return sortName(s);
		default:
			rawPrintln(sym);
	}
}

@doc{Give the sort of a tree.

	@param t	A tree
	@return		The tree's (non-terminal) sort}
public str sortOf(Tree t) {
	assert notAmb(t);
	return sortName(t.prod.rhs);
}

public str sortName(Symbol sym) {
	switch(sym) {
		case \cf(s):
			return sortName(s);
		case \lex(s):
			return "lex" + sortName(s);
		case sort(s):
			return s;
		case \parameterized-sort(s, as):
			return "<s>[[<strJoin([sortName(t) | t <- as], ",")>]]";
		case iter(s):
			return "<sortName(s)>+";
		case \iter-star(s):
			return "<sortName(s)>*";
		case opt(s):
			return "<sortName(s)>?";
	}
}

public str baseSortName(str s) {
	if(/^<name:[a-zA-Z_]*>/ := s)
		return name;
	else
		return s;
}

public bool notAmb(Tree t) {
	if(amb(alts) := t) { 
		println("Ambiguity detected:");
		for(a <- alts) {
			println("    <a.prod.rhs> ::= <a.prod.lhs ? a.prod>");
		}
		a = getOneFrom(alts);
		println("  In sentence: \'<a>\'");
		return false;
	}
	else
		return true;	 
}
