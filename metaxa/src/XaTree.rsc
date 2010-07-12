module XaTree
import ParseTree;
import XaLib;
import List;
import IO;
import String;
import Set;
import Node;
import Map;

@doc{Datatype for abstract trees}
data XaTree =
	  cons(str name, list[XaTree] args, str sort)  // term constructor
	| seq(list[XaTree] args, str sort)             // list
	| leaf(str strVal, str sort)                   // leaf / string
	| inject(XaTree arg, str sort)                 // injection (not used)
	| termvar(str name, str sort);					   // variable

@doc{Tokens for concrete syntax annotations of trees}
data XaToken =
	  token(str chars)    // a literal
	| space(str chars)   // whitespace
	| comment(str chars)  // non-whitespace layout
	| child(int index)   // reference to child node
	| sep(XaToken tok, str chars);

anno list[XaToken] XaTree@concrete;

anno loc XaTree@location;

data XaImplodeOpts =
	implodeOptions(bool foldInjections, bool addConcrete, bool addLocation, list[str] varConses);

XaImplodeOpts defaultOpts = implodeOptions(true, false, false, ["RascalVar", "RascalListVar"]);

public XaTree implodeTree(Tree t) { return implodeTree(t, defaultOpts); }

public XaTree implodeTree(Tree t, XaImplodeOpts opts) {
	XaTree result;
	list[XaToken] concrete = [];
	switch(t) {
		// leaf nodes
		case \appl(\prod([\lex(_)], \cf(s), _), args): {
			result = leaf(unparse(args[0]), sortName(s));
			concrete = [token(unparse(args[0]))];
		}
		// injection
		// TODO: should deal with attributes
		case \appl(\prod([\cf(_)], \cf(s), \no-attrs()), args): {
			result = implodeTree(args[0], opts);
			if(!opts.foldInjections) {
				result = inject(result, s);
				concrete = [child(0)];
			}
		}
		// an injection which gets the type of the outer production
		case \appl(\prod([\cf(_)], \cf(s), \attrs([_*,\term(\abstract()),_*])), args): {
			result = implodeTree(args[0], opts);
			if(opts.foldInjections)
				result.sort = sortName(s);
			else {
				result = inject(result, s);
				concrete = [child(0)];
			}
		}
		// alternative choice
		case \appl(\prod(_, \cf(\alt(_,_)), _), [arg]): {
			return implodeTree(arg, opts);
		}
		// option -- none
		case \appl(\prod([], \cf(\opt(s)), _), []): {
			result = seq([], sortName(s));
			concrete = [];
		}
		// option -- some
		case \appl(\prod([_], \cf(\opt(s)), _), [arg]): { 
			result = seq([implodeTree(arg, opts)], sortName(s));
			concrete = [child(0)];
		}
		// normal node
		case \appl(\prod(\lhs,\cf(s),_), args): { 
			name = consOf(t) ? sortName(s);
			if(name in opts.varConses) {
				//println(consOf(t), t);
				<astArgs, _> = implodeArgList(args);
				if(name == "RascalListVar")
					result = termvar(unparse(astArgs[1]) + "*", unparse(astArgs[0]));
				else
					result = termvar(unparse(astArgs[1]), unparse(astArgs[0]));
			}
			else {
				<astArgs, concrete> = implodeArgList(args);
				result = cons(name, astArgs, sortName(s));
			}
		}
		// list/sequence
		case \appl(\list(s), args): { 
			<astArgs, concrete> = implodeArgList(args);
			result = seq(astArgs, sortOfList(s));
		}
		// ambiguity
		// TODO: don't use println for reporting
		case \amb(alts): { 
			println("Ambiguity detected:");
			for(a <- alts) {
				println("    <a.prod.rhs> ::= <a.prod.lhs ? a.prod>");
			}
			a = getOneFrom(alts);
			println("  In sentence: \'<a>\'");
			println("  Proceeding with:\n    <a.prod.rhs> ::= <a.prod.lhs ? a.prod>");
			result = implodeTree(getOneFrom(alts), opts);
		}	 
		// anything we don't recognize yet
		default: {
			rawPrintln("Unknown AsFix: ", t);
			result = leaf(unparse(t), sortOf(t));
		}
	}

	if(opts.addLocation)
		result@location = t@\loc;
	
	if(opts.addConcrete && concrete != [])
		result@concrete = concrete;
		
	return result;
}

private tuple[list[XaTree], list[XaToken]] implodeArgList(list[Tree] ts) {
	list[XaTree] astArgs = [];
	list[XaToken] cstArgs = [];
	int j = 0;

	if(size(ts) == 0)
		return <[], []>;
	for(i <- [0 .. size(ts)-1])
		try {
			switch(ts[i].prod.rhs ? []) {
				case \cf(opt(\layout())): {
					l = unparse(ts[i]);
					if(false && /^<before:\s*><com:\S(.|\r|\n)*\S><after:\s*>$/ := l) {
						if(before != "")
							cstArgs += space(before);
						if(com != "")
							cstArgs += comment(com);
						if(after != "")
							cstArgs += space(after);
					}
					else
							cstArgs += space(l);
				}
				case lit(_):
					cstArgs += token(unparse(ts[i]));
				default: {
					astArgs += implodeTree(ts[i]);
					cstArgs += child(j);
					j += 1;
				}
			}
		}
		catch e: {
			println("Error: ", substring("<[ts[i]]>", 0, 200));
			throw e;
		}
	return <astArgs, cstArgs>;
}

public str toTerm(XaTree tree) {
	switch(tree) {
		case cons(c, as, _):
			return c + "(" + strJoin([toTerm(a) | a <- as], ", ") + ")";
		case seq(as, _):
			return "[" + strJoin([toTerm(a) | a <- as], ", ") + "]";
		case inject(a, _):
			return toTerm(a);
		case leaf(l, _):
			return "\'" + l + "\'";
	}
}

public str unparse(XaTree tree) {

	switch(<tree, tree@concrete ? []>) {
		case <cons(c, as, _), []>:
			return strJoin([unparse(a) | a <- as], " ");
		case <seq(as, _), []>:
			return strJoin([unparse(a) | a <- as], "\n");
		case <inject(a, _), []>:
			return unparse(a);
		case <leaf(l, _), []>:
			return l;
	}
			
	str result = "";
	for(tok <- tree@concrete)
		switch(tok) {
			case token(t):
				result += t;
			case space(l):
				result += l;
			case comment(c):
				result += c;
			case child(i):
				result += unparse(tree.args[i]);
		}
	return result;
}

public str unparse(list[XaTree] trees, str sep) {
	return strJoin([unparse(t) | t <- trees], sep);
}

public XaTree reindent(XaTree tree) {
	return reindent(tree, "", "")[0];
}

set[str] vertSorts = {"Decl", "Stat", "ModuleHead"};
set[str] sectSorts = {"Decl", "ModuleHead"};
set[str] dropSpaceAfter = {"(", "[", "{", "."};
set[str] dropSpaceBefore = {"(", ")", "[", "]", "{", "}", ".", ",", ";", ""};

str suppressSpace = strJoin(["[(\\[{.].", ".[,;.}\\])]", ".\\(", "."], "|");
str alwaysSpace = strJoin(["\\w\\w"], "|");

public tuple[XaTree, str, str] reindent(XaTree tree, str indent, str lastChar) {
	switch(<tree, tree@concrete ? []>) {
		case <cons(c, as, _), []>:
			tree@concrete = [child(i) | i <- [0 .. size(as)], i < size(as)];
		case <seq(as, _), []>:
			tree@concrete = [child(i) | i <- [0 .. size(as)], i < size(as)];
		case <inject(a, t), []>:
			return inject(reindent(a, indent, lastChar), t);
		case <leaf(l, _), []>:
			return <tree, substring(l, 0, 1) ? "", substring(l, size(l)-1) ? lastChar>;
		default:
			tree@concrete = [x | x <- tree@concrete, space(_) !:= x];
	}
	
	
	list[XaToken] concPat = [];
	list[XaToken] spaces = [];
	list[XaToken] toks = [];
	str firstChar;
	for(tok <- tree@concrete) {
		if(concPat != [])
			spaces = [space(" ")];
		if(lastChar == "\n") {
			if(indent != "")
				concPat += [space(indent)];
			spaces = [];
			lastChar = "\t";
		}

		/* track the last char seen + the first char of this token, to determine spacing */
		chars = lastChar;
		toks = [tok];

		switch(tok) {
			case space(l):
					spaces = [];
			case comment(c): {
				firstChar = firstChar ? "#";
				chars += "#";
				toks += space("\n");
				lastChar = "\n";
			}
			case token(t): {
				//if(lastChar in dropSpaceAfter)
				//	space = [];
				//if((substring(t, 0, 1) ? "") in dropSpaceBefore)
				//	space = [];
				lastChar = substring(t, size(t)-1) ? lastChar;
				if(size(t) > 0) {
					firstChar = firstChar ? substring(t, 0, 1);
					chars += substring(t, 0, 1);
				}
			}
			case child(i): {
				str fc;

				if(seq(_,s) := tree.args[i] && s in vertSorts) {
					if(s in sectSorts)
						toks = [space("\n"), tok];
					<tree.args[i], fc, lastChar> = reindent(tree.args[i], indent + "\t", lastChar);
				}
				else if(cons(_,_,s) := tree.args[i] && s in vertSorts) {
					if(lastChar != "\t") { // add newline and indent
						toks = [space((lastChar == "\n") ? indent : "\n" + indent), tok];
						spaces = [];
					}
					<tree.args[i], fc, _> = reindent(tree.args[i], indent, "\t");
					toks += space("\n");
					if(s in sectSorts)
						toks += space("\n");
					lastChar = "\n";
				}
				else {
					<tree.args[i], fc, lastChar> = reindent(tree.args[i], indent, lastChar);
				}
				firstChar = firstChar ? fc;
				chars += fc;
			}
		}

		if(/^(<suppressSpace>)$/ := chars && /^(<alwaysSpace>)$/ !:= chars)
			spaces = []; 
		concPat += spaces;
		concPat += toks;
	}
//	concPat = [cp | cp <- concPat, layout("") !:= cp];
	return <tree[@concrete = concPat], firstChar ? "", lastChar>;
}

public str toString(XaTree tree) {
	return toString(tree, true);
}

public XaTree baseSorted(XaTree tree) {
	return visit(tree) {
		case t:cons(name, args, /^<s:[a-zA-Z_]+>\[\[/) => cons(name, args, s)[@concrete=t@concrete] ? cons(name, args, s)
		case t:seq(args, /^<s:[a-zA-Z_]+>\[\[/) => seq(args, s)[@concrete=t@concrete] ? seq(args, s)
		case t:leaf(arg, /^<s:[a-zA-Z_]+>\[\[/) => leaf(arg, s)
		case t:inject(arg, /^<s:[a-zA-Z_]+>\[\[/) => inject(arg, s)
		case t:termvar(name, /^<s:[a-zA-Z_]+>\[\[/) => termvar(name, s)
	}
}
public str toString(XaTree tree, bool dropAnnos) {
	str s;

	switch(tree) {
		case cons(name, args, srt):
			s = "cons(\"<name>\", [<strJoin([toString(a, dropAnnos) | a <- args], ", ")>], \"<srt>\")";
		case seq(args, srt):
			s = "seq([<strJoin([toString(a, dropAnnos) | a <- args], ", ")>], \"<srt>\")";
		case leaf(string, srt):
			s = "leaf(\"<string>\", \"<srt>\")";
		case inject(arg, srt):
			s = "inject(<toString(arg, dropAnnos)>, \"<srt>\")";
		case termvar(name, srt):
			s = name;
	}

	if(!dropAnnos) {
		annos = getAnnotations(tree);
		if(size(annos) > 0) {
			s += "[";
			for(k <- domain(annos))
				s += "@<k>=<annos[k]>";
			s += "]";
		}
	}
	return s;
}

