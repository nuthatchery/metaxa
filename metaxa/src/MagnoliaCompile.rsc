module MagnoliaCompile
import String;
import IO;
import Set;
import List;
import XaLib;
import XaTree;

data MagnoliaEnv = environ(rel[Name, XaTree] members);
data Name = qual(str, Name) | name(str);

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

public set[XaTree] membersOf(Name n, MagnoliaEnv env) {
	return env.members[n];
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
public str flattenModule(XaTree tree, MagnoliaEnv ctx) {
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

//public tuple[XaTree, MagnoliaEnv] loadModule(str moduleName, str project) {
//	path = findInPath("<moduleName>.mg", project);
//	println("Loading module <moduleName> from <path>");
//	return loadFile(path, project);
//}

//public tuple[XaTree, MagnoliaEnv] loadFile(loc moduleLoc, str project) {
//	result = newEnv();
//	XaTree tree = parseMagnolia(moduleLoc);
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

public MagnoliaEnv loadTrees(list[XaTree] trees) {
	result = newEnv();
	for(tree <- trees)
		result = loadTree(tree, result);
	return result;
}

public MagnoliaEnv loadTree(XaTree tree, MagnoliaEnv result) {
	if(cons("MagnoliaTree", [cons("ModuleHead", [NAME, seq(CLAUSES,_)], _), seq(DECLS,_)], _) := tree) {
		for(decl <- DECLS) {
			// println(decl);
			switch(decl) {
				case cons("DefDecl", [cons("ConceptClause", [CONCEPT], _), _, cons(_, [seq(CONTENTS,_)], _)], _): {
					println(name(unparse(CONCEPT)));
					result.members += {name(unparse(CONCEPT))} * toSet(CONTENTS);
				}
			}
		}
	}
	return result;
}

public XaTree flattenConcepts(XaTree tree, MagnoliaEnv env) {
	return top-down-break visit(tree) {
		case cDecl : cons("DefDecl", [cons("ConceptClause", [CONCEPT], _), seq(SUBCLAUSES,_), cons("DeclBody", [seq(CONTENTS,_)], _)], _) : {
			println("Looking at <unparse(CONCEPT)>: ");
			set[XaTree] decls = {};
			for(decl <- CONTENTS) {
				println("  Decl: ", decl);
				switch(decl) {
					case cons("Requires", [seq(reqs, _)], _):
						for(req  <- reqs) {
							println("  <unparse(CONCEPT)> requires <unparse(req)>:");
							<ds, env> = getConcept(req, env);
							decls += ds;
						}
					default:
						decls += decl;
				}
			}
			list[XaTree] declList = sortList(toList(decls));
			cDecl.args[2].args[0].args = declList;
			cDecl.args[2].args[0]@concrete = [];
			insert cDecl;
		}
		case decl : cons("DefDecl", [cons("ImplClause", [IMPL, CONCEPT], _), seq(SUBCLAUSES,_), cons("DeclBody", [seq(CONTENTS,_)],_)], _) : {
			<ds, env> = getConcept(CONCEPT, env);
			println("Implementation: <unparse(IMPL)>");
			println(" on signature: <[unparse(d) | d <- ds]>");
			decl.args[0].args[1] = cons("ConceptBody", [cons("DeclBody", [seq(sortList(toList(ds)),"Decl")], "DeclBody")[@concrete = [token("{"), child(0), token("}")]]], "ConceptExpr");
			insert decl;
		}
		case decl: cons("ModelsConcept", [NAME, CONCEPT1, BY, CONCEPT2], _): {
			<ds1, env> = getConcept(CONCEPT1, env);
			<ds2, env> = getConcept(CONCEPT2, env);
			println("Satisfaction: <unparse(NAME)>");

			decl.args[1] = cons("ConceptBody", [cons("DeclBody", [seq(sortList(toList(ds1)),"Decl")], "DeclBody")[@concrete = [token("{"), child(0), token("}")]]], "ConceptExpr");
			decl.args[3] = cons("ConceptBody", [cons("DeclBody", [seq(sortList(toList(ds2)),"Decl")], "DeclBody")[@concrete = [token("{"), child(0), token("}")]]], "ConceptExpr");
			insert decl;
		}
		case decl: cons("ModelsImpl", [NAME, IMPL, BY, CONCEPT], _): {
			<ds, env> = getConcept(CONCEPT, env);
			println("Satisfaction: <unparse(NAME)>");

			decl.args[3] = cons("ConceptBody", [cons("DeclBody", [seq(sortList(toList(ds)),"Decl")], "DeclBody")[@concrete = [token("{"), child(0), token("}")]]], "ConceptExpr");
			insert decl;
		}
		//case cDecl: cons(c,as,"Decl") => cDecl
	}
}

public tuple[set[XaTree], MagnoliaEnv] getConcept(XaTree cExpr, MagnoliaEnv ctx) {
	set[XaTree] result = {};
	switch(cExpr) {
		case cons("Concept", [NAME], _): {
			decls = membersOf(name(unparse(NAME)), ctx);
			if(decls == {})
				println("Concept empty, or not found: <unparse(NAME)>");
			for(decl <- decls)
				switch(decl) {
					case cons("Requires", [seq(reqs, _)], _):
						for(req <- reqs) {
							<ds, ctx> = getConcept(req, ctx);
							if(size(ds) > 0) {
								ctx.members -= {<name(unparse(NAME)), decl>};
								ctx.members += {name(unparse(NAME))} * ds;
								result += ds;
							}
							else {
								result += decl;
							}
						}
					default:
						result += decl;
				}
		}
		case cons("MorphedConcept", [C, M], _): {
			<decls, ctx> = getConcept(C, ctx);
			result = morph(decls, M);
		}
		case cons("ConceptBody", [cons("DeclBody", [seq(decls,_)], _)], _): {
			for(decl <- decls)
				switch(decl) {
					case cons("Requires", [seq(reqs, _)], _):
						for(req <- reqs) {
							<ds, ctx> = getConcept(req, ctx);
							if(size(ds) > 0)
								result += ds;
							else
								result += decl;
						}
					default:
						result += decl;
				}
		}
		default:
			println("Unknown concept kind: <unparse(cExpr)>");
	}
	return <result, ctx>;
}

public &T morph(&T tree, XaTree M) {
	map[XaTree,XaTree] renaming = ();
	rel[XaTree, list[XaTree], XaTree] inlineDefs = {};
//	for(t <- tree)
//		println("Input: ", unparse(t));
	switch(M) {
		case cons("Morphisms", [seq(MS, _)], _):
			for(cons("Rename", [X, Y], _) <- MS)
				renaming += (X : Y);
		case cons("Morphism", [cons("FunClause",[NAME, cons("Dummy", [seq(PARAMS, _)], _), TYPE], _), EXPR], "MorphClause"): {
			println("<NAME> should be inlined");
			inlineDefs += {<NAME, PARAMS, EXPR>};
		}
		case cons("Morphism", [MC : cons(_,_,"MorphClause")], _):
			return morph(tree, MC);
		case cons("Rename", [X, Y], _):
			renaming += (X : Y);
	}

//	for(t <- tree)
//		println("Rename: ", unparse(t));
	return applyInlining(tree, inlineDefs, renaming);
}

&T applyInlining(&T tree, rel[XaTree, list[XaTree], XaTree] inlineDefs, map[XaTree, XaTree] renaming) {
	return top-down-break visit(tree) {
		case app: cons("Apply", [cons("Fun", [funName], _), seq(args, _)], _): {
			inlineables = inlineDefs[funName];
			if(size(inlineables) > 0) {
				done = false;
				for(<params, body> <- inlineables, size(params) == size(args)) {
					if(done)
						println("Whoops: ambiguity in function call -- needs overload resolution:\n\t<unparse(funName)>(<unparse(args, ", ")>)");
					map[XaTree, XaTree] subst = ();
					args = applyInlining(args, inlineDefs, renaming);
					for(<cons("Param", [paramName, paramType], _), arg> <- [<params[i], args[i]> | i <- domain(params)])
						subst[paramName] = arg;
					body = applyInlining(body, {}, subst);
					println("<unparse(app)> =\> <unparse(body)>");
					insert body;
				}
			}
			else
				fail;
		}
		case x : cons(_,_,"Identifier"):
			if(x in renaming)
				insert renaming[x];
			else
				fail;
	}
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

public list[XaTree] sortList(list[XaTree] lst)
{
  if(size(lst) <= 1){
  	return lst;
  }
  
  list[XaTree] less = [];
  list[XaTree] greater = [];
  XaTree pivot = lst[0];
  
  <pivot, lst> = takeOneFrom(lst);
  
  for(XaTree elm <- lst){
     if(leq(elm, pivot)){
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

public bool leq(XaTree t1, XaTree t2) {
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

public XaTree setModuleName(XaTree tree, str name) {
	rawPrintln(tree.args[0].args[0]);
	tree.args[0].args[0] = cons("Name", [leaf(name, "ID")], "Name");
	return tree;
}
