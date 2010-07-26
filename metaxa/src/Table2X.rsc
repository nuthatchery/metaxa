module Table2X
import ATermIO;
import IO;
import XaLib;
import XaTree;
import String;
import Node;
import List;
import Set;
import ValueIO;
/*
  import Table2X;
  generateFromSyntax("Magnolia", |project://MetaXa/src/Magnolia.trm|);
*/
data RuntimeException = WrongParseTableFormatError(loc location);

public node readParseTable(loc location) {
	if(node n := readTextATermFile(location)) {
		if(n[0] == 6)
			return n;
		else
			throw WrongParseTableFormatError(location);
	}
	else
		throw WrongParseTableFormatError(location);
} 

public void generateFromSyntax(str language, loc location) {
	tree = readParseTable(location);
	list[value] prods = [];
	map[str,tuple[list[XaToken],str,str]] ppTable = ();
	set[str] deprecated = {};
	astDefs = "";
	mkAstDefs = "";
	set[str] synMap = {};
	if(list[value] labels := tree[2]) {
		for("label"(p, _) <- labels) {
			switch(p) {
				case "prod"(_,"lit"(_),_):
					;
				case "prod"(_,"cf"("iter-star"(_)),_):
					;
				case "prod"(_,"cf"("iter"(_)),_):
					;
				case "prod"(_,"cf"("iter-sep"(_,_)),_):
					;
				case "prod"(_,"cf"("iter-star-sep"(_,_)),_):
					;
				case "prod"(_,"cf"("opt"("layout"())),_):
					;
				case "prod"(_,"cf"("layout"()),_):
					;
				case "prod"(_,"lex"(_),_):
					;
				case "prod"(["lex"(s)], "cf"(s), _):
					;
				case "prod"([], "cf"("opt"(_)), _):
					;
				case "prod"(["cf"(s)], "cf"("opt"(s)), _):
					;
				case "prod"(_, "seq"(_), _):
					;
				case p:"prod"([lhs0*], rhs0, attr): {
					list[node] attrs = [];
					if("attrs"([as*]) := attr)
						attrs = toNodeList(as);
						
					lhs = toNodeList(lhs0);
					rhs = toNode(rhs0);
					syn = getSyn(lhs, rhs, attrs);
					prods += p;
					if(getCons(attrs) != "") {
						<consname, concrete, conssort> = getPP(lhs, rhs, attrs);
						if(consname in ppTable && ppTable[consname][0] != concrete) {
							if(consname in deprecated) {
								if(!hasAttr("deprecated", attrs)) {
									ppTable[consname] = <concrete, syn, conssort>;
									deprecated -= consname;
								}
								else
									println("Overlapping deprecated syntax for <consname>: <concrete>");
							}
							else if(!hasAttr("deprecated", attrs)) {
								println("Overlapping syntax for <consname>: <concrete>");
								println("  -------------- Using <consname>: < ppTable[consname][0]>");
							}
						}
						else {
							ppTable[consname] = <concrete, syn, conssort>;
							if(hasAttr("deprecated", attrs))
								deprecated += consname;
						}
							
								
						<t0,t1> = getADT(lhs, rhs, attrs);
						astDefs += t0;
						mkAstDefs += t1;
						synMap += getSyntaxMapping(lhs, rhs, attrs);
					}
					//else
					//	println(getInj(lhs, rhs, attrs));
				}
			}
		}
	}
	
	rascalFile = location;
	rascalFile.path = replaceLast(location.path, "/[^/]*", "/<language>AST.rsc");
	writeFile(rascalFile, "module <language>AST\n\n<astDefs>\n" +
			"data AST = leaf(str strVal) | var(str name) | seq(list[AST] args);\n\n" +
			"data XaToken = token(str chars) | space(str chars) | comment(str chars) | child(int index) | sep(XaToken tok, str chars);\n" +
			"anno loc AST@\\loc;\n" +
			"anno list[XaToken] AST@concrete;\n" +
			"AST makeAST(str name, list[AST] args) {\nswitch(name) {\n<mkAstDefs>\n}\n}\n");
//	javaPP = "";
//	for(consname <- ppTable)
//		javaPP += "\t\t// <ppTable[consname][1]>\n\t\ttbl.put(\"<consname>\", <ppTable[consname][0]>);\n\n";
	ppFile = location;
	ppFile.path = replaceLast(location.path, "/[^/]*", "/<language>SkinTable.ptf");
	ppText = "(\n";
	for(c <- ppTable) {
		if(ppText != "(\n")
			ppText+= ",\n";
		ppText += "  \"<c>\" : <ppTable[c]>";
	}
	ppText += "\n)\n";
	writeFile(ppFile, ppText);
	ppFile.path = replaceLast(location.path, "/[^/]*", "/<language>SkinTable.pbf");
	writeBinaryValueFile(ppFile, ppTable);
//	writeFile(javaFile, "package org.magnolialang.xatree;\n\nimport java.util.HashMap;\nimport java.util.Map;\nimport org.eclipse.imp.pdb.facts.IList;\n"
//		+ "import static org.magnolialang.xatree.XaTreeFactory.*;\n\n"
//		+ "class <language>SkinTable {\n\tpublic static Map\<String,IList\> getMap() {\n"
//		+ "\t\tMap\<String,IList\> tbl = new HashMap\<String,IList\>();\n\n<javaPP>\t\treturn tbl;\n\t}\n}\n");
	mxaFile = location;
	mxaFile.path = replaceLast(location.path, "/[^/]*", "/<language>Mapping.mxa");
	writeFile(mxaFile, "language module <language>\n\nsyntax skin\n\n<strJoin(sort(toList(synMap)),"")>\n\n");	
}

public node toNode(value v) {
	if(node n := v)
		return n;
	else
		rawPrintln(v);
}

public list[node] toNodeList(list[value] vs) {
	return [n | node n <- vs];
}

public str getCons(list[value] vs) {
	for(v <- vs) {
		if("term"("cons"(c)) := v)
			return c;
		}
	return "";
}

public bool hasAttr(str name, list[value] vs) {
	for(v <- vs) {
		if("term"(t) := v && node n := t)
			if(getName(n) == name)
				return true;
		}
	return false;
}

public tuple[str,list[XaToken],str] getPP(list[value] lhs, value rhs, list[value] attrs) {
	list[XaToken] pp = [];
	int chld = 0;
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit):
				pp += token("<lit>");
			case "cf"("opt"("layout"())):
				pp += space(" ");
			case "cf"("iter-sep"(s,"lit"(lit))): {
				pp += sep(child(chld), "<lit>");
				chld = chld + 1;
			}
			case "cf"("iter-star-sep"(s,"lit"(lit))): {
				pp += sep(child(chld), "<lit>");
				chld = chld + 1;
			}
			default: {
				pp += child(chld);
				chld = chld + 1;
			}
		}
	}
	
	return <"<getCons(attrs)>/<chld>", pp, pTblSortName(rhs)>;//"vf.list(<strJoin(["<p>" | p <- pp], ", ")>)">;
}

public tuple[str,str] getADT(list[value] lhs, value rhs, list[value] attrs) {
	list[str] defs = [];
	list[str] args = [];
	int chld = 0;
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit):
				;
			case "cf"("opt"("layout"())):
				;
			case s: {
				defs += "AST"; //rascalSortName(sort);
				args += "arg<chld>";
				chld = chld + 1;
			}
		}
	}
	consname = getCons(attrs);
	return <left("data AST = <consname>(<strJoin(defs, ", ")>);", 70) + "  // <getSyn(lhs, rhs, attrs)>\n",
			"\t\tcase \<\"<consname>\", [<strJoin(args, ", ")>]\>: return <consname>(<strJoin(args, ", ")>);\n">;			
}

public str getInj(list[node] lhs, node rhs, list[node] attrs) {
	//if(["cf"(s1)] := lhs && "cf"(s2) := rhs)
	//		return "\talias <rascalSortName(s2)> = <rascalSortName(s1)>;";
	return "";
}

public str getSyn(list[value] lhs, value rhs, list[value] attrs) {
	list[str] syn = [];
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit):
				syn += "\"<lit>\"";
			case "cf"("opt"("layout"())):
				syn += " ";
			case s: {
				syn += pTblSortName(s);
			}
		}
	}
	
	return "<pTblSortName(rhs)> ::= <strJoin(syn, "")>"; 
}

public str getSyntaxMapping(list[value] lhs, value rhs, list[value] attrs) {
	list[str] lhsPP = [];
	list[str] rhsPP = [];
	int chld = 0;
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit): {
				if(/^[a-z]+$/ := lit)
					rhsPP += "<lit>";
				else
					rhsPP += "\"<lit>\"";
				lhsPP += "<lit>";
			}
			case "cf"("opt"("layout"())): {
				lhsPP += " ";
				rhsPP += " ";
			}
			default: {
				lhsPP += pTblSortName(sym);
				rhsPP += pTblSortName(sym);
			}
		}
	}
	
	return left("\t<pTblSortName(rhs)>(: <strJoin(lhsPP, "")> :)", 50) + " -\> (+ <strJoin(rhsPP, "")> +)\n\n";
}


public str pTblSortName(value sym) {
	if(node n := sym) {
		switch(n) {
			case "cf"(s):
				return pTblSortName(s);
			case "lex"(s):
				return "lex-" + sortName(s);
			case "sort"(s):
				return s;
			case "parameterized-sort"(s, as):
				return "<s>[[<strJoin([pTblSortName(t) | t <- as], ",")>]]";
			case "iter"(s):
				return "<pTblSortName(s)>+";
			case "iter-star"(s):
				return "<pTblSortName(s)>*";
			case "iter-sep"(s,sp):
				return "{<pTblSortName(s)> <pTblSortName(sp)>}+";
			case "iter-star-sep"(s,sp):
				return "{<pTblSortName(s)> <pTblSortName(sp)>}*";
			case "opt"(s):
				return "<pTblSortName(s)>?";
			case "lit"(s):
				return "\"<s>\"";
			case "seq"([ss*]):
				return "(<strJoin([pTblSortName(s) | s <- ss], " ")>)";
			default: {
				println("Whoops: ", sym);
				return "<sym>";
			}
		}
	}
	rawPrintln(sym);
}

public str rascalSortName(value sym) {
	if(node n := sym) {
		switch(n) {
			case "cf"(s):
				return rascalSortName(s);
			case "lex"(s):
				return "\\lex-" + sortName(s);
			case "sort"(s):
				return s;
			case "parameterized-sort"(s, as):
				return "<s>[<strJoin([rascalSortName(t) | t <- as], ",")>]";
			case "iter"(s):
				return "list[<rascalSortName(s)>]";
			case "iter-star"(s):
				return "list[<rascalSortName(s)>]";
			case "iter-sep"(s,_):
				return "list[<rascalSortName(s)>]";
			case "iter-star-sep"(s,_):
				return "list[<rascalSortName(s)>]";
			case "opt"(s):
				return "list[<rascalSortName(s)>]";
			case "lit"(_):
				return "";
			case "seq"([sorts*]): {
				ss = [y | y <- [rascalSortName(x) | x <- sorts], y != ""];
				if([] := ss)
					return "";
				else if([s] := ss)
					return s;
				else
					return "tuple[<strJoin(ss, ", ")>]";
			}
			default:
				println("Whoops!");
		}
	}
	rawPrintln(sym);
}
