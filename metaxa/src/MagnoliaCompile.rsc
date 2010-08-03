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
import MagnoliaCompileLib;
import TopLevelOps;

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
	set[AST] result;
	if(Name(_) := n)
		result = ctx.members[_,_,n,_];
	else if(QName(_,_) := n)
		result = ctx.members[_,n,_,_];
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

public str flattenModule(AST tree, MagnoliaEnv ctx) {
	return unparse(reindent(flattenConcepts(tree, ctx)));
}

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
		case DeclBody(seq(ds)):
			result =  DeclBody(seq(flattenDeclList(ds, ctx)));
		case Define(_, _, _, _):
			result = DeclBody(seq(flattenDeclList([body], ctx)));
		case n: Name(_):
			result =  flattenTopExpr(kind, lookup(n, ctx), ctx) ? addMark(body, "Top-level name <n> not defined");
		case n: QName(_,_):
			result =  flattenTopExpr(kind, lookup(n, ctx), ctx) ? addMark(body, "Top-level name <n> not defined");
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
			result =  morph(flattenTopExpr(kind, expr, ctx), morphism, ctx);
		case Renamed(a, r):
			result = morph(flattenTopExpr(kind, a, ctx), r, ctx);
		case Protected(expr, protect):
			result =  Protected(flattenTopExpr(kind, expr, ctx), protect);
		case OnDefines(a, defs):
			result =  preserveAnnos(decls(sigUnion(fullOf(defs),
				 {setModifier(d, Require()) | d <- fullOf(flattenTopExpr(kind, a, ctx))})),
				 body);
		case Defines(defs):
			;
		case External(ext):
			result = external(kind, ext, ctx);
		case At(a, b):
			result = applyImpl(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx), body);
		case AtAt(a, b):
			result = applyImpl(flattenTopExpr(kind, a, ctx), flattenTopExpr(kind, b, ctx), body);
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
					if(DeclBody(seq([d1, ds1*])) := req) {
						result += {d1[@mark = ((d1@mark ? {}) + (req@mark ? {}))]};
						result += toSet(ds1);
					}
					else if(DeclBody(seq([])) := req)
						result += {Nop()[@mark = (req@mark ? {})]};
					else if(Name(_) := req || QName(_,_) := req || hasMarkRecursive(req)) {
						result += Requires(seq([req]));
					}
					else {
								println("  no expansion found: <trunc(req)>");
						result += Requires(seq([req])); // {Requires(seq([addMark(req, "Unable to flatten expression", "internal")]))};
					}
				}
			default:
				result += decl;
		}
	}
	return sortList(toList(result));
}



public AST morph(AST tree, AST morphism, MagnoliaEnv ctx) {
	if(DeclBody(_) := tree) {
		map[AST,AST] renaming = ();
		rel[AST, str, list[AST], AST] inlineDefs = {};

		if(Name(_) := morphism || QName(_,_) := morphism) {
			morphism = flattenTopExpr("implementation", morphism, ctx);
			println("Flattened to <trunc(morphism)>");
		}
		switch(morphism) {
			case Define(_,_,_,_): {
				inlineDefs = getInlineDefs(morphism);
			}
			case DeclBody(seq(_)): {
				inlineDefs = getInlineDefs(morphism);
			}
			case Renamed(a, r):
				return morph(flattenTopExpr("any", a, ctx), r, ctx);
			case seq([rn*]): {
				for(Rename(x, y) <- rn) {
					renaming += (x : y);
				}
			}
			default: {
				println("Unknown morphism: <morphism>");
				return Morphed(tree, morphism); // addMark(morphism, "Unimplemented morphism type \'<getName(morphism)>\'"));
			}
		}
		return addMarks(applyInlining(tree, inlineDefs, renaming));
	}
	else // (probably) unable to morph anything else
		return tree;
}

public AST external(str kind, AST expr, MagnoliaEnv ctx) {
	empty = DeclBody(seq([]));
	AST result;
	switch(expr) {
		case ExternalDefines(language, name, defPart):
			result = makeExternal(language, name, flattenTopExpr(kind, defPart, ctx));
		case ExternalOnDefines(language, name, onPart, defPart):
			result = decls(sigUnion(
				fullOf(makeExternal(language, name, flattenTopExpr(kind, defPart, ctx))),
				{setModifier(d, Require()) | d <- fullOf(flattenTopExpr(kind, onPart, ctx))}));
		case ExternalExtendsOnDefines(language,name,
							extends,onPart, makeExternal(language, name, defPart)):
			result = decls(sigUnion(sigUnion(
				fullOf(makeExternal(language, name, flattenTopExpr(kind, defPart, ctx))),
				{setModifier(d, Require()) | d <- fullOf(flattenTopExpr(kind, onPart, ctx))}),
				{setModifier(d, Extend()) | d <- fullOf(flattenTopExpr(kind, extends, ctx))}));
			 
		default:
			throw InternalError("Unknown external expression <getName(expr)>", expr);
		}
	return preserveAnnos(result, expr);
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

