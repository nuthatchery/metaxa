module MagnoliaCompile
import String;
import IO;
import Set;
import List;
import XaLib;
//import XaTree;
import MagnoliaAST;
import Node;
alias envRelation = rel[AST sort, AST qname, AST name, AST attrs, AST def];
data MagnoliaEnv = environ(envRelation members);
data RuntimeException = LookupError(AST name);
anno str AST@error;
public MagnoliaEnv newEnv() {
	return environ({});
}

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
		println("Ambiguous name <n>");
		throw LookupError(n);
	}
	else {
		println("Name <n> not found");
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
				println("Defined <def[0]> <def[1]>");
				result.members += {def};
			}
		}
	}
	return result;
}

public AST flattenConcepts(AST tree, MagnoliaEnv env) {
	if(MagnoliaTree(ModuleHead(moduleName, seq(CLAUSES)), seq(DECLS)) := tree) {
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
	switch(body) {
		case Decls(DeclBody(seq(ds))):
			return Decls(DeclBody(seq(flattenDeclList(ds, ctx))));
		case Morphed(expr, morphism):
			return morph(flattenTopExpr(kind, expr, ctx), morphism);
		case n: Name(_): {
			return flattenTopExpr(kind, lookup(n, ctx), ctx) ? body[@error="Top-level name <n> not defined"];
		}
		case Models(a, b):
			return Models(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx));
		case WithModels(a, w, b):
			return Models(flattenTopExpr(kind, a, ctx),
						flattenTopExpr(kind, w, ctx),
						flattenTopExpr(kind, b, ctx));
		default:
			return body;
	}
}

public list[AST] flattenDeclList(list[AST] ds, MagnoliaEnv ctx) {
	set[AST] result = {};
	
	for(decl <- ds) {
		switch(decl) {
			case Requires(seq(reqs)):
				for(req <- reqs) {
					println("Requires <trunc(req)>");
					req = flattenTopExpr("concept", req, ctx);
					if(Decls(DeclBody(seq(ds1))) := req) {
						println("  decls added");
						result += toSet(ds1);
					}
					else {
						println("  no expansion found: <trunc(req)>");
						result += {Requires(seq([req]))[@error="Unable to flatten expression"]};
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

public &T morph(&T tree, AST morphism) {
	map[AST,AST] renaming = ();
	rel[AST, list[AST], AST] inlineDefs = {};

	switch(morphism) {
		case ExprDef(_,FunClause(n,Dummy(seq(as)),t),_,body): {
			inlineDefs = {<n, as, body>};
		}
		case seq([rn*]): {
			for(Rename(x, y) <- rn) {
				println("Rename: <x>, <y>");
				rename += (x : y);
			}
		}
		default: {
			println("Unknown morphism: <morphism>");
			return Morphed(tree, morphism[@error="Unknown morphism"]);
		}
	}
	return applyInlining(tree, inlineDefs, renaming);
}

&T applyInlining(&T tree, rel[AST, list[AST], AST] inlineDefs, map[AST, AST] renaming) {
	return top-down-break visit(tree) {
		case app: Apply(Fun(funName), seq(args)): {
			<_, params, body> = overload(funName, inlineDefs, args);
			if(body != Nop()) {
				map[AST, AST] subst = ();
				args = applyInlining(args, inlineDefs, renaming);
				for(<Param(paramName, paramType), arg> <- [<params[i], args[i]> | i <- domain(params)])
					subst[paramName] = arg;
				body = applyInlining(body, {}, subst);
				println("<app> =\> <body>");
				insert body;
			}
			else
				fail;
		}
		case NoDefDecl(_,FunClause(funName,Dummy(seq(args)),_),_): {
			<n, _, _> = overload(funName, inlineDefs, args);
			if(n != Nop())
				insert Nop();
			else
				fail;
		}
		case DefDecl(_,FunClause(funName,Dummy(seq(args)),_),_,_): {
			<n, _, _> = overload(funName, inlineDefs, args);
			if(n != Nop())
				insert Nop();
			else
				fail;
		}
	}
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
