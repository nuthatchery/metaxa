module MagnoliaCompile
import String;
import IO;
import Set;
import List;
import Map;
import XaLib;
//import XaTree;
import MagnoliaAST;
import Node;
alias envRelation = rel[AST sort, AST qname, AST name, AST attrs, AST def];
data ErrorMark = Mark(str severity, str message, list[loc] locs);
data MagnoliaEnv = environ(envRelation members);
data RuntimeException = LookupError(AST name);
anno set[ErrorMark] AST@mark;
public MagnoliaEnv newEnv() {
	return environ({});
}

map[str,str] severityMsg = (
	"warning" : "Warning",
	"error" : "Error",
	"internal" : "Internal Error",
	"info" : "Info");

public MagnoliaEnv joinEnv(MagnoliaEnv env1, MagnoliaEnv env2) {
	env = newEnv();
	env.members = env1.members + env2.members;
	return env;
}

public MagnoliaEnv joinEnv(list[MagnoliaEnv] envs) {
	env = newEnv();
	for(e <- envs) {
		env.members += e.members;
	}
	return env;
}

public set[AST] membersOf(AST n, MagnoliaEnv env) {
	return env.members[n];
}

public AST lookup(AST n, MagnoliaEnv ctx) {
	result = ctx.members[_,_,n,_];
	if(size(result) == 1)
		return getOneFrom(result);
	else if(size(result) > 1) {
		println("Ambiguous overloaded name <n>");
		throw LookupError(n);
	}
	else {
		// println("Name <n> not found");
		throw LookupError(n);
	}
}


//public Tree compileMagnolia(loc file) {
//	Tree parsed = parseMagnolia(file);
//	//Tree desugared = desugarMagnolia(parsed);
//
//	decls = loadTree(parsed, file);
//	return flattenConcepts(parsed, decls);
//}
//public str flattenModule(loc path, str project) {
//	<tree, ctx> = loadFile(path, project);
//	return unparse(reindent(flattenConcepts(tree, ctx)));
//}
public str flattenModule(AST tree, MagnoliaEnv ctx) {
	return unparse(reindent(flattenConcepts(tree, ctx)));
}
//public void flattenModule(str moduleName, str project) {
//	path = findInPath("<moduleName>.mg", project);
//	<tree, ctx> = loadFile(path);
//	path.path = replaceLast(path.path, "\\.mg", "-flattened.mg");
//	println("Writing flattened module to <path>");
//	writeFile(path, unparse(reindent(flattenConcepts(tree, ctx))));
//}

//public void flattenModuleToCxx(str moduleName, str project) {
//	path = findInPath("<moduleName>.mg", project);
//	<tree, ctx> = loadFile(path, project);
//	path.path = replaceLast(path.path, "\\.mg", ".cc");
//	println("Writing flattened module to <path>");
//	writeFile(path, unparse(reindent(cxxFormat(flattenConcepts(tree, ctx)))));
//}

//public tuple[AST, MagnoliaEnv] loadModule(str moduleName, str project) {
//	path = findInPath("<moduleName>.mg", project);
//	println("Loading module <moduleName> from <path>");
//	return loadFile(path, project);
//}

//public tuple[AST, MagnoliaEnv] loadFile(loc moduleLoc, str project) {
//	result = newEnv();
//	AST tree = parseMagnolia(moduleLoc);
//	if(cons("MagnoliaTree", [cons("ModuleHead", [NAME, seq(CLAUSES,_)], _), seq(DECLS,_)], _) := tree) {
//		for(clause <- CLAUSES) {
//			switch(clause) {
//				case cons("Imports", [seq(MODULES, _)], _):
//					for(cons("ImportAll", [m], _) <- MODULES) {
//						result = joinEnv(result, loadModule(unparse(m), project)[1]);
//					}
//			}
//		}
//		for(decl <- DECLS) {
//			// println(decl);
//			switch(decl) {
//				case cons("DefDecl", [cons("ConceptClause", [CONCEPT], _), _, cons(_, [seq(CONTENTS,_)], _)], _): {
//					result.members += {name(unparse(CONCEPT))} * toSet(CONTENTS);
//				}
//			}
//		}
//	}
//	return <tree, result>;
//}

public MagnoliaEnv loadTrees(list[AST] trees) {
	result = newEnv();
	for(tree <- trees)
		result = loadTree(tree, result);
	return result;
}

public MagnoliaEnv loadTree(AST tree, MagnoliaEnv result) {
	if(MagnoliaTree(ModuleHead(moduleName, seq(CLAUSES)), seq(DECLS)) := tree) {
		for(decl <- DECLS) {
			// println(decl);
			tuple[AST sort, AST qname, AST name, AST attrs, AST def] def;
			found = true;
			switch(decl) {
				case ConceptDef(mods, name, subcls, body):
					def = <leaf("concept"), qualify(moduleName, name), name, subcls, body>;
				case SatisfactionDef(mods, name, subcls, body):
					def = <leaf("satisfaction"), qualify(moduleName, name), name, subcls, body>;
				case ImplDef(mods, name, subcls, body):
					def = <leaf("implementation"), qualify(moduleName, name), name, subcls, body>;
				case LibraryDef(mods, name, subcls, body):
					def = <leaf("library"), qualify(moduleName, name), name, subcls, body>;
				case Nop():
					found = false;
				default: {
					println("Unknown top-level definition: <getName(decl)>");
					found = false;
				}
			}
			if(found) {
				result.members += {def};
			}
		}
	}
	return result;
}

public AST flattenConcepts(AST tree, MagnoliaEnv env) {
	if(MagnoliaTree(ModuleHead(moduleName, seq(CLAUSES)), seq(DECLS)) := tree) {
		println("Flattening module <moduleName>");
		list[AST] flatDecls = [];
	
		for(decl <- DECLS) {
			switch(decl) {
				case ConceptDef(mods, name, subcls, body):
					decl[3] = flattenTopExpr("concept", body, env); 
				case SatisfactionDef(mods, name, subcls, body):
					decl[3] = flattenTopExpr("satisfaction", body, env); 
				case ImplDef(mods, name, subcls, body):
					decl[3] = flattenTopExpr("implementation", body, env); 
				case LibraryDef(mods, name, subcls, body):
					decl[3] = flattenTopExpr("library", body, env); 
			}
			flatDecls += decl;
		}

		tree.arg1[0] = flatDecls;
	}
	return tree;
}

public AST flattenTopExpr(str kind, AST body, MagnoliaEnv ctx) {
	result = body;
	switch(body) {
		case Decls(DeclBody(seq(ds))):
			result =  Decls(DeclBody(seq(flattenDeclList(ds, ctx))));
		case DefDeclNS(_, _, _, _):
			result = Decls(DeclBody(seq(flattenDeclList([body], ctx))));
		case NoDefDeclNS(_, _, _):
			result = Decls(DeclBody(seq(flattenDeclList([body], ctx))));
		case n: Name(_): {
			result =  flattenTopExpr(kind, lookup(n, ctx), ctx) ? addMark(body, "Top-level name <n> not defined");
		}
		case Models(a, b):
			result =  Models(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case WithModels(a, w, b):
			result =  WithModels(flattenTopExpr(kind, a, ctx),
						flattenTopExpr(kind, w, ctx),
						flattenTopExpr(kind, b, ctx));
		case SignatureOf(a):
			result =  SignatureOf(flattenTopExpr("any", a, ctx));
		case FullOf(a):
			result =  FullOf(flattenTopExpr("any", a, ctx));
		case DeclaredOf(a):
			result =  DeclaredOf(flattenTopExpr("any", a, ctx));
		case Filtered(a, f):
			result =  Filtered(flattenTopExpr("any", a, ctx), f);
		case OnFilter(a, f):
			result =  OnFilter(flattenTopExpr("any", a, ctx), f);
		case DeclaredFilter(a, f):
			result =  DeclaredFilter(flattenTopExpr("any", a, ctx), f);
		case Morphed(expr, morphism):
			result =  morph(flattenTopExpr(kind, expr, ctx), morphism);
		case Renamed(a, r):
			result = morph(flattenTopExpr(kind, a, ctx), r);
		case Protected(expr, protect):
			result =  Protected(flattenTopExpr(kind, expr, ctx), protect);
		case OnDefines(a, defs):
			result =  OnDefines(flattenTopExpr(kind, a, ctx), defs);
		case Defines(defs):
			;
		case External(ext):
			;
		case At(a, b):
			result = At(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case AtAt(a, b):
			result = AtAt(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case Plus(a, b):
			result = Plus(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case PlusPlus(a, b):
			result = PlusPlus(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case Times(a, b):
			result = Times(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case Times(a, b):
			result = TimesTimes(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case DataInvariant(a, b):
			result = DataInvariant(flattenTopExpr(kind, a, ctx), b);
		case Quotient(a, b):
			result = Quotient(flattenTopExpr(kind, a, ctx), b);
		case Homomorphism(a, b, c, d):
			result = Homomorphism(flattenTopExpr(kind, a, ctx),
				flattenTopExpr(kind, b, ctx),
				flattenTopExpr(kind, c, ctx),
				flattenTopExpr(kind, d, ctx));
		default:
			println("Unknown top expr: <trunc(body)>");
	}
	
	return preserveAnnos(result, body);
}

public list[AST] flattenDeclList(list[AST] ds, MagnoliaEnv ctx) {
	set[AST] result = {};
	
	for(decl <- ds) {
		switch(decl) {
			case Requires(seq(reqs)):
				for(req <- reqs) {
					req = flattenTopExpr("concept", req, ctx);
					if(Decls(DeclBody(seq([d1, ds1*]))) := req) {
						result += {d1[@mark = ((d1@mark ? {}) + (req@mark ? {}))]};
						result += toSet(ds1);
					}
					else if(Decls(DeclBody(seq([]))) := req)
						result += {Nop()[@mark = (req@mark ? {})]};
					else if(Name(_) := req) {
						result += req;
					}
					else {
						//			println("  no expansion found: <trunc(req)>");
						result += {Requires(seq([addMark(req, "Unable to flatten expression", "internal")]))};
					}
				}
			default:
				result += decl;
		}
	}
	return sortList(toList(result));
}

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
public AST morph(AST tree, AST morphism) {
	if(Decls(_) := tree) {
		map[AST,AST] renaming = ();
		rel[AST, list[AST], AST] inlineDefs = {};
	
		switch(morphism) {
			case DefDeclNS(_,FunClause(n,Dummy(seq(as)),t),_,body): {
				inlineDefs = {<n, as, body>};
			}
			case Decls(DeclBody(seq(ds))): {
				for(DefDecl(_,FunClause(n,Dummy(seq(as)),t),_,body) <- ds)
					inlineDefs = {<n, as, body>};
			}
			case seq([rn*]): {
				for(Rename(x, y) <- rn) {
					renaming += (x : y);
				}
			}
			default: {
				println("Unknown morphism: <morphism>");
				return Morphed(tree, addMark(morphism, "Unknown morphism"));
			}
		}
		return addMarks(applyInlining(tree, inlineDefs, renaming));
	}
	else // (probably) unable to morph anything else
		return tree;
}

data InlineInfo = inlineInfo(
		rel[AST, list[AST], AST] inlineDefs,
		map[AST, AST] renaming,
		set[AST] usedRenamings,
		set[AST] usedInlines,
		set[AST] foundDecls,
		set[ErrorMark] marks);
		
tuple[AST, set[ErrorMark]] applyInlining(AST tree, rel[AST, list[AST], AST] inlineDefs, map[AST, AST] renaming) {
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

tuple[AST, InlineInfo] applyInliningInternal(AST tree, InlineInfo info) {
	result = top-down-break visit(tree) {
		case app: Apply(Fun(funName), seq(args)): {
			<_, params, body> = overload(funName, info.inlineDefs, args);
			if(body != Nop()) {
				map[AST, AST] subst = ();
				<args0, info> = applyInliningInternal(seq(args), info);
				if(seq(inlinedArgs) := args0) {
					for(<Param(paramName, paramType), arg> <- [<params[i], inlinedArgs[i]> | i <- domain(params)])
						subst[paramName] = arg;
					<body, _> = applyInlining(body, {}, subst);
					// println("<app> =\> <body>");
					info.usedInlines += {funName};
					insert preserveAnnos(body, app);
				}
					
			}
			else
				fail;
		}
		case NoDefDecl(_,FunClause(funName,Dummy(seq(args)),_),_): {
			<n, _, _> = overload(funName, info.inlineDefs, args);
			if(n != Nop()) {
				info.foundDecls += {funName};
				insert Nop();
			}
			else
				fail;
		}
		case DefDecl(_,FunClause(funName,Dummy(seq(args)),_),_,_): {
			<n, _, _> = overload(funName, info.inlineDefs, args);
			if(n != Nop()) {
				info.foundDecls += {funName};
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
	}
	return <result, info>;
}

public tuple[AST,list[AST],AST] overload(AST name, rel[AST,list[AST],AST] defs, list[AST] args) {
	result = <Nop(),[],Nop()>;
	cands = defs[name];
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

public AST addMark(AST tree, str msg) {
	return addMark(tree, msg, "error", []);
}

public AST addMark(AST tree, str msg, str severity) {
	return addMark(tree, msg, severity, []);
}

public AST addMark(AST tree, str msg, str severity, list[loc] locs) {
	if(locs == [])
		locs = [tree@\loc] ? []; 
	oldMarks = tree@mark ? {};
	return tree[@mark = (oldMarks + {Mark(severity, "<severityMsg[severity]>: <msg>", locs)})];
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg) {
	return addMark(marks, msg, "error", []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, str severity) {
	return addMark(marks, msg, severity, []);
}

public set[ErrorMark] addMark(set[ErrorMark] marks, str msg, str severity, list[loc] locs) {
	return marks + {Mark(severity, "<severityMsg[severity]>: <msg>", locs)};
}

public AST addMarks(tuple[AST, set[ErrorMark]] treeAndMark) {
	oldMarks = treeAndMark[0]@mark ? {};
	return treeAndMark[0][@mark = (oldMarks + treeAndMark[1])];
}
 
public loc findInPath(str name, str project) {
	searchPath = 
		[ |project://<project>/Fundamentals/|,
		  |project://<project>/Mathematics/|,
		  |project://<project>/Indexable/|,
		  |project://<project>/Array/|,
		  |project://<project>/Magnolia-langauge/|,
		  |project://<project>/Mathematics/|,
		  |project://<project>/io/|
		  ];
	for(dir <- searchPath) {
		filePath = dir;
		filePath.path += name;
		if(exists(filePath))
			return filePath;
	}
}

public list[AST] sortList(list[AST] lst)
{
  if(size(lst) <= 1){
  	return lst;
  }
  
  list[AST] less = [];
  list[AST] greater = [];
  AST pivot = lst[0];
  
  <pivot, lst> = takeOneFrom(lst);
  
  for(AST elm <- lst){
     if(elm <= pivot){
       less = [elm] + less;
     } else {
       greater = [elm] + greater;
     }
  }
  
  return sortList(less) + pivot + sortList(greater);
}

map[str, int] declOrder = (
	"SignatureClause" : -20,
	"ConceptClause" : -20,
	"ImplClause" : -20,
	"ModelsImpl" : -20,
	"TypeClause" : -10,
	"VarClause" : -8,
	"FunClause" : -6,
	"PredClause" : -6,
	"ProcClause" : -4,
	"AxiomClause" : -2
);

public bool leq(AST t1, AST t2) {
	if(cons(_, [cons(declClause1,[name1,x1*],_),y1*], _) := t1
	&& cons(_, [cons(declClause2,[name2,x2*],_),y2*], _) := t2)
	{
		declOrder1 = declOrder[declClause1] ? 0;
		declOrder2 = declOrder[declClause2] ? 0;

		if(declOrder1 == declOrder2) {
			nameStr1 = unparse(name1);
			nameStr2 = unparse(name2);
			if(nameStr1 == nameStr2)
				return t1 <= t2;
			else
				return nameStr1 < nameStr2;
		}
		else
			return declOrder1 < declOrder2;
	}
	else
		return t1 <= t2;
}

public AST setModuleName(AST tree, str name) {
	//rawPrintln(tree.arg0);
	tree.arg0.arg0 = Name(leaf(name));
	return tree;
}

public AST qualify(AST moduleName, AST name) {
	if(Name(n) := name)
		return QName(moduleName, n);
}
