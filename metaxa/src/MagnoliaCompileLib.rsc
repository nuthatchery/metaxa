module MagnoliaCompileLib
import String;
import IO;
import Set;
import List;
import Map;
import XaLib;
import MagnoliaAST;
import Node;
alias envRelation = rel[AST sort, AST qname, AST name, AST attrs, AST def];
data ErrorMark = Mark(str severity, str message, list[loc] locs);
data MagnoliaEnv = environ(envRelation members);
data RuntimeException = LookupError(AST name)
	| InternalError(str msg, AST ast);
anno set[ErrorMark] AST@mark;
public loc unknownLoc = |unknown:///|;

map[str,str] severityMsg = (
	"warning" : "Warning",
	"error" : "Error",
	"internal" : "Internal Error",
	"info" : "Info");

public str trunc(AST tree) {
	s = "<tree>";
	if(size(s) > 80)
		return substring(s, 0, 80);
	else
		return s;
}


public AST preserveAnnos(AST tree, AST orig) {
	if(tree == orig)
		return tree;
	treeAnnos = getAnnotations(tree);
	origAnnos = getAnnotations(orig);
	for(a <- origAnnos) {
		if(a notin treeAnnos) {
			if(a == "concrete") {
				if(getName(tree) == getName(orig))
					treeAnnos[a] = origAnnos[a];
			}
			else
				treeAnnos[a] = origAnnos[a];
		}
	}
	return setAnnotations(tree, treeAnnos);
}



public AST addMark(AST tree, str msg) {
	return addMark(tree, msg, "error", []);
}

public AST addMark(AST tree, str msg, str severity) {
	return addMark(tree, msg, severity, []);
}

public AST addMark(AST tree, str msg, str severity, list[loc] locs) {
	if(locs == [] || locs == [unknownLoc])
		locs = [tree@\loc] ? []; 
	oldMarks = tree@mark ? {};
	return tree[@mark = (oldMarks + {Mark(severity, "<severityMsg[severity]>: <msg>", locs)})];
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg) {
	return addMark(marks, msg, "error", []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, AST locSource) {
	return addMark(marks, msg, "error", [locSource@\loc] ? []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, str severity) {
	return addMark(marks, msg, severity, []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, str severity, AST locSource) {
	return addMark(marks, msg, severity, [locSource@\loc] ? []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, str severity, list[loc] locs) {
	return marks + {Mark(severity, "<severityMsg[severity]>: <msg>", locs)};
}

public AST addMarks(tuple[AST, set[ErrorMark]] treeAndMark) {
	oldMarks = treeAndMark[0]@mark ? {};
	return treeAndMark[0][@mark = (oldMarks + treeAndMark[1])];
}

public AST addMarks(set[ErrorMark] marks, AST tree) {
	oldMarks = tree@mark ? {};
	return tree[@mark = (oldMarks + marks)];
}
 
public bool hasMark(AST tree) {
	return size((tree@mark ? {})) > 0;
}

public bool hasMarkRecursive(AST tree) {
	visit(tree) {
		case AST t:
			if(size((t@mark ? {})) > 0)
				return true;
	}
	return false;
}

public loc locOf(AST tree) {
	return tree@\loc ? unknownLoc;
}

@doc{Return true if the 'contained' location is textually within the 'container'.
The URIs must match exactly, and the contained area must not extend outside
the container. If the 'container' has no offset/length information, it is
assumed to extend to the entire file. Line/column information is ignored.}
public bool isIn(loc contained, loc container) {
	return sameFile(contained, container)
		&&  ((contained.offset >= container.offset 
						&& (contained.offset + contained.length) <= (container.offset + container.length))
			|| (container.offset == -1 && container.length == -1));	
}

public bool sameFile(loc loc1, loc loc2) {
	return loc1 != unknownLoc && loc2 != unknownLoc
		&& loc1.scheme == loc2.scheme
		&&	loc1.authority == loc2.authority
		&&	loc1.host == loc2.host
		&&	loc1.port == loc2.port
		&&	loc1.path == loc2.path
		&&	loc1.extension == loc2.extension
		&&	loc1.fragment == loc2.fragment
		&&	loc1.user == loc2.user;
}

public str locInfo(loc location, loc base) {
	str filename = "";
	if(!sameFile(location, base))
		filename = location.path + ":";
	if(location.begin.line == location.end.line)
		return "<filename><location.begin.line>:<location.begin.column>-<location.end.column>";
	else
		return "<filename><location.begin.line>:<location.begin.column>-<location.end.line>:<location.end.column>";
}

public str nameOf(AST tree) {
	switch(tree) {
		case Define(_,d,_,_):
			return nameOf(d);
		case TypeClause(n):
			return nameOf(n);
		case FunClause(n, _, _):
			return nameOf(n);
		case ProcClause(n, _):
			return nameOf(n);
		case Name(leaf(n)):
			return n;
		case QName(n1, n2):
			return "<nameOf(n1)>.<nameOf(n2)>";
	}
}


public AST qualify(AST moduleName, AST name) {
	if(Name(n) := name) {
		return QName(moduleName, n);
	}
}

public list[AST] paramsToArgs(AST paramList) {
	if(ParamList(seq(pList)) := paramList) {
		for(i <- domain(pList)) {
			if(Param(n, t) := pList[i])
				pList[i] = Var(n, t);
			else if(Param(m, n, t) := pList[i])
				pList[i] = Var(n, t);
			else
				throw InternalError("Unknown parameter constructor <getName(pList[i])>", pList[i]);
		}
		return pList;
	}
	else
		throw InternalError("Unknown parameter list constructor <getName(paramList)>", paramList);
}



public tuple[&T, set[ErrorMark]] inline(&T tree, set[AST] decls) {
	rel[AST, str, list[AST], AST] inlineDefs = {};
	for(d <- decls)
		inlineDefs += getInlineDefs(d);
	println(inlineDefs);
	return applyInlining(tree, inlineDefs, ());
}
public rel[AST, str, list[AST], AST] getInlineDefs(AST decl) {
	switch(decl) {
		case Define(_,dcl,_,body0): {
			if(BodyS(body) := body0 || BodyNS(body) := body0 || body:Block(_) := body0) {
				switch(dcl) {
					case FunClause(n,ParamList(seq(as)),t):
						return {<n, "fun", as, body>};
					case ProcClause(n,ParamList(seq(as)),t):
						return {<n, "proc", as, body>};
					case TypeClause(n):
						return {<n, "type", [], body>};
				}
			}
		}
		case DeclBody(seq(ds)): {
			return ({} | it + getInlineDefs(d) | d <- decl);
		}
	}
	return {};
}

data InlineInfo = inlineInfo(
		rel[AST, str, list[AST], AST] inlineDefs,
		map[AST, AST] renaming,
		set[AST] usedRenamings,
		set[AST] usedInlines,
		set[AST] foundDecls,
		set[ErrorMark] marks);
		
public tuple[&T, set[ErrorMark]] applyInlining(&T tree, rel[AST, str, list[AST], AST] inlineDefs, map[AST, AST] renaming) {
	<result, info> = applyInliningInternal(tree, inlineInfo(inlineDefs, renaming, {}, {}, {}, {})); 
	set[ErrorMark] marks = info.marks;
	for(n <- info.inlineDefs<0>) {
		if(n notin info.foundDecls)
			marks = addMark(marks, "Inlined operation <n> not declared in body\n",
					"warning", [n@\loc]?[]);
		if(n notin info.usedInlines)
			marks = addMark(marks, "Inlined operation <n> not used in body\n",
					"warning", [n@\loc]?[]);
	}

	for(n <- domain(info.renaming)) {
		if(n notin info.usedRenamings)
			marks = addMark(marks, "Renamed name <n> not used in body\n",
					"warning", [n@\loc]?[]);
	}
	
	return <result, marks>;
}

tuple[&T, InlineInfo] applyInliningInternal(&T tree, InlineInfo info) {
	result = top-down-break visit(tree) {
		case app: Apply(Fun(funName), seq(args)): {
			<_, params, body> = overload(funName, "fun", info.inlineDefs, args);
			if(body != Nop()) {
				map[AST, AST] subst = ();
				<args0, info> = applyInliningInternal(seq(args), info);
				if(seq(inlinedArgs) := args0) {
					for(<Param(paramName, paramType), arg> <- [<params[i], inlinedArgs[i]> | i <- domain(params)]) {
						subst[paramName] = arg;
					}
					<body, _> = applyInlining(body, {}, subst);
					// println("<app> =\> <body>");
					info.usedInlines += {funName};
					insert preserveAnnos(body, app);
				}
					
			}
			else
				fail;
		}
		case typ: Type(typeName): {
			<n, _, body> = overload(typeName, "type", info.inlineDefs, []);
			if(body != Nop()) {
				println("<typ> =\> <body>");
				info.usedInlines += {typeName};
				insert preserveAnnos(body, typ);
			}
			else
				fail;
		}
		case Define(_,FunClause(funName,ParamList(seq(args)),_),_,_): {
			<n, _, _> = overload(funName, "fun", info.inlineDefs, args);
			if(n != Nop()) {
				info.foundDecls += {funName};
				insert Nop();
			}
			else
				fail;
		}
		case Define(_,TypeClause(typeName),_,_): {
					<n, _, _> = overload(typeName, "type", info.inlineDefs, []);
			if(n != Nop()) {
				info.foundDecls += {typeName};
				insert Nop(); 
			}
			else
				fail;
		}
		case n: Name(_): {
			if(n in info.renaming) {
				info.usedRenamings += {n};
				insert info.renaming[n];
			}
			else
				fail;
		}
		case v: Var(n,_): {
			if(n in info.renaming) {
				info.usedRenamings += {n};
				insert info.renaming[n];
			}
			else
				fail;
		}
		case v: UserSyntax2(n): {
			if(n in info.renaming) {
				info.usedRenamings += {n};
				insert info.renaming[n];
			}
			else
				fail;
		}
	}
	return <result, info>;
}

public tuple[AST,list[AST],AST] overload(AST name, str kind, rel[AST,str,list[AST],AST] defs, list[AST] args) {
	result = <Nop(),[],Nop()>;
	cands = defs[name,kind];
	if(size(cands) > 0) {
		done = false;
		for(<params, body> <- cands, size(params) == size(args)) {
			if(done)
				println("Whoops: ambiguity in function call -- needs overload resolution:\n\t<unparse(funName)>(<unparse(args, ", ")>)");
			else
				result = <name, params, body>;	
		}
	}
	return result;
}
