module RascalAbstractBackend

import XaLang;
import String;
import XaLib;
import ParseTree;
import XaTree;

public str pp(Decl d) {
	switch(d) {
		case ruleDef(ps,c):
			throw IllegalASTFormat("Ruledef found in backend tree:\n" + strJoin([pp(p) | p <- ps], "\n") + "\n----\n" + pp(c));
		case opDef(n,rt,as,body):
			return "public " + rt + " " + n + "(" + strJoin([typeOf(a) + " " + pp(a) | a <- as], ", ") + ") {\n" + ppIndent(pp(body)) + "}\n";
		case opDecl(n,rt,ps):
			return ""; // "public " + rt + " " + n + "(" + strJoin([p | p <- ps], ", ") + ");";
		default:
			throw IllegalASTFormat("Don\'t know what to do with decl \'<d>\'");
	}
}

public str pp(list[Decl] ds) {
	return replaceAll(strJoin([pp(d) | d <- ds], "\n"), "\t", "    ");
}
public str pp(Clause s) {
	switch(s) {
		case dtrans(l,o,r):
			throw IllegalASTFormat("Transition found in backend tree: <pp(l)> ==<pp(o,"=")>\> <pp(r)>");
		case trans(lhs,ops,rhs):
			throw IllegalASTFormat("Transition found in backend tree: <pp(lhs)> --<pp(ops,"-")>\> <pp(rhs)>");
		case condMatch(p,e,t,nop()):
			return "if(" + pp(p) + " := " +  pp(e) + ")\n" + ppIndent(pp(t));
		case condMatch(p,e,t,a):
			return "if(" + pp(p) + " := " +  pp(e) + ")\n" + ppIndent(pp(t)) + "else\n" + ppIndent(pp(a));
		case condRel(name, args, t, nop()):
			return "if(relationHolds(\"" + name + "\", \<" + strJoin([pp(a) | a <- args], ",") + "\>))\n" + ppIndent(pp(t));
		case condRel(name, args, t, e):
			return "if(relationHolds(\"" + name + "\", \<" + strJoin([pp(a) | a <- args], ",") + "\>))\n" + ppIndent(pp(t)) + "else\n" + ppIndent(pp(e));
		case relate(name, args):
			return "setRelation(\"" + name + "\", \<" + strJoin([pp(a) | a <- args], ",") + "\>);\n";
		case nop():
			return ";\n";
		case block(cs):
			return "{\n" + ppIndent(pp(cs)) + "}\n";
		case ret(v):
			return "return " + pp(v) + ";\n";
		case failure():
			return "throw Failure();\n";
		default:
			throw IllegalASTFormat("Don\'t know what to do with clause \'<s>\'");
	}
}

public str pp(list[Clause] cs) {
	return strJoin([pp(x) | x <- cs], "");
} 

public str pp(list[Expr] ops,str sep) {
	return ("" | it + pp(o) + sep | o <- ops);
}

public str pp(Expr e) {
	switch(e) {
		case var(v,t):
			return v; // + ":" + t;
		case app(n,as,t):
			return n + "(" + strJoin([pp(a) | a <- as], ", ") + ")";
		case dqPattern(p,t):
			return toString(implodeTree(p));
		case sqPattern(p,t):
			return toString(implodeTree(p));
		case seq(es,t):
			return t + "[" + strJoin([pp(a) | a <- as], ", ") + "]";
		case tup(es):
			return "\<" + strJoin([pp(a) | a <- as], ", ") + "\>";
		case foreach(v, s, r):
			return "[" + pp(r) + " | " + pp(v) + " \<- " + s + "]";
		default:
			throw IllegalASTFormat("Don\'t know what to do with expr \'<e>\'");
	}
}

str ppIndent(str s) {
	return "\t" + replaceAll(replaceAll(s,"\n","\n\t"), "\t+$", "");
}

public str ppMatch(Expr pat, Expr obj, Clause consq, Clause alt) {
	return "try {\n\t" + ppMatch(pat, obj, consq) + "}\ncatch(Failure f) {\n\t" + pp(alt) + "}\n";
}

public str ppMatch(Expr pat, Expr obj, Clause consq) {
	switch(pat) {
		case var(v,t):
			return v; // + ":" + t;
		case app(n,as,t):
			return n + "(" + strJoin([pp(a) | a <- as], ", ") + ")";
		case dqPattern(p,t):
			return ppMatch(p, obj, consq);
		case sqPattern(p,t):
			return ppMatch(p, obj, consq);
		case seq(es,t):
			return t + "[" + strJoin([pp(a) | a <- as], ", ") + "]";
		case tup(es):
			return "\<" + strJoin([pp(a) | a <- as], ", ") + "\>";
		case foreach(v, s, r):
			return "[" + pp(r) + " | " + pp(v) + " \<- " + s + "]";
		default:
			throw IllegalASTFormat("Don\'t know what to do with expr \'<e>\'");
	}
}
