module XaRules

import Meta::MetaSyntax;
import IO;
import XaLib;
import String;
import List;
import Set;
import RascalBackend;
import XaLang;

public tuple[list[Decl], set[Type]] implodeRules(list[Tree] rules) {
	list[Decl] rs = [];
	set[Type] sorts = {};

	visit(rules) {
		case \cf(sort(s)):
			sorts += {s};
	}

	for(r <- rules) {
		switch(r) {
			case (MXaRuleDef)`<MXaRuleClause+ PREMISES> <LINE> <MXaRuleConclusion CONCLUSION>`:
				rs += implodeRule(PREMISES, CONCLUSION, r);
			case (MXaOpDecl)`<NAME>: <{MXaType ","}* TYPES> -> <TYPE>`:
				rs += opDecl("<NAME>", "<TYPE>", ["<t>" | t <- TYPES]);
			case appl(prod(_,\cf(opt(\layout())),_),_):
				;
			default:
				throw IllegalTreeFormat("implodeRules: unknown rule declaration: <r>", r);
		}
	}

	return <rs, sorts>; 
}

public Decl implodeRule(Tree* premises, Tree conclusion, Tree r) {
	return ruleDef([implodeClause(p) | p <- premises], implodeConclusion(conclusion));
}

public list[Clause] implodeConclusion(Tree c) {
	switch(c) {
		case (MXaRuleConclusion)`<MXaRuleClause CLAUSE>`:
			return [implodeClause(CLAUSE)];
		case (MXaRuleConclusion)`<MXaRuleClause CLAUSE>, <{MXaRel ","}+ RELS>`:
			return [implodeClause(CLAUSE), [implodeClause(r) | MXaRel r <- RELS]];
		default:
			throw IllegalTreeFormat("implodeConclusion: unknown clause: <c>", c);
 	}
}

public Clause implodeClause(Tree c) {
	switch(c) {
		case (MXaRuleClause)`<MXaExpr LHS> -- <{MXaOp "-"}+ OPS> -> <MXaExpr RHS>`:
			return trans(implodeExpr(LHS), implodeOps(OPS), implodeExpr(RHS));
		case (MXaRuleClause)`<MXaExpr LHS> ==> <MXaExpr RHS>`:
			return dtrans(implodeExpr(LHS), [], implodeExpr(RHS));
		case (MXaRuleClause)`<MXaExpr LHS> ==<{MXaOp "="}+ OPS> => <MXaExpr RHS>`:
			return dtrans(implodeExpr(LHS), implodeOps(OPS), implodeExpr(RHS));
		case (MXaRuleClause)`<MXaCons C><<{MXaExpr ","}* AS>>`:
			return relate("<C>", [implodeExpr(a) | a <- AS]);
		case (MXaRel)`<MXaCons C><<{MXaExpr ","}* AS>>`:
			return relate("<C>", [implodeExpr(a) | a <- AS]);
		default:
			throw IllegalTreeFormat("implodeClause: unknown clause: <c>", c);
 	}
}

public Expr implodeExpr(Tree e) {
	switch(e) {
		case (MXaExpr)`<TQuoted PAT>`:
			return dqPattern(unquote(PAT), sortOf(unquote(PAT)));
		case (MXaExpr)`<DQuoted PAT>`:
			return dqPattern(unquote(PAT), sortOf(unquote(PAT)));
		case (MXaExpr)`<SQuoted PAT>`:
			return sqPattern(unquote(PAT), sortOf(unquote(PAT)));
		case (MXaExpr)`<MXaCons C>(<{MXaExpr ","}* AS>)`:
			return app("<C>", [implodeExpr(a) | a <- AS], "?");
		case (MXaExpr)`<MXaVar V>`:
			return var("<V>", "?");
		default:
			throw IllegalTreeFormat("implodeExpr: unknown expr: <e>", e);
	}
}

public list[Expr] implodeOps(Tree* ops) {
	list[Expr] r = [];
	for(o <- ops) {
		switch(o) {
			case (MXaOp)`<MXaCons C>`:
				r = r + app("<C>",[],"?");
			case (MXaOp)`<MXaCons C>(<{MXaExpr ","}* AS>)`:
				r = r + app("<C>",[],"?");
			case appl(prod(_,lit(_),_),_):
				;
			case appl(prod(_,\cf(opt(\layout())),_),_):
				;
			default:
				throw IllegalTreeFormat("implodeOps: unknown operator: <o>", o);

		}
	}
	return r;
}


public Decl desugarXa(Decl d) {
	return visit(d) {
		case ruleDef(ps,[dtrans(lhs,[],rhs), cs*]): {
				body = desugarPremises(ps, block([cs, ret(rhs)]));
				insert opDef("resolve","?",[lhs],body);
		}
		case ruleDef(ps,[trans(lhs,[app(o,args,_)],rhs), cs*]): {
				body = desugarPremises(ps, block([cs, ret(rhs)]));
				insert opDef(o,"?",[lhs] + args, body);
		}
		case ruleDef(ps,cs): {
				body = desugarPremises(ps, block(cs));
				insert opDef("relate","?",[], body);
		}
		case ruleDef(ps,c): {
			throw IllegalASTFormat("Don\'t know what to do with rule conclusion <c>");
		}
	}
}

public Clause desugarPremises(list[Clause] cs, Clause conclusion) {
	if([c, cs*] := cs) {
		switch(c) {
			case dtrans(lhs,list[Expr] ops,rhs):
				return desugarPremises([trans(lhs, [app("scope", [], "?"), ops, app("resolve", [], "?"), app("unscope", [], "?")], rhs), cs], conclusion);
			case trans(lhs,list[Expr] ops,rhs):
				return condMatch(rhs, (lhs | app(o, [it] + as, t) | app(o, as, t) <- ops), desugarPremises(cs, conclusion), failure());
			case relate(name, args):
				return condRel(name, args, desugarPremises(cs, conclusion), failure());
			case condMatch(pat,expr,t,e):
				return condMatch(pat, expr, desugarPremises([t, cs], conclusion), desugarPremises([e, cs], conclusion));
			case condRel(name, args, t, e):
				return condRel(name, args,  desugarPremises([t, cs], conclusion), desugarPremises([e, cs], conclusion));
			case nop():
				return desugarPremises(cs, conclusion);
			case ret(e):
				return ret(e);
			case block(cs):
				return block(desugarPremises(cs, conclusion));		
		}
	}
	else 
		return conclusion;
}

public list[Decl] desugarXa(list[Decl] ds) {
	return [desugarXa(d) | d <- ds];
}

public tuple[list[Decl], set[Type]] desugarXa(tuple[list[Decl], set[Type]] dts) {
	if(<ds, ts> := dts)
		return <[desugarXa(d) | d <- ds], ts>;
}
 
public list[Decl] compileXa(list[Tree] decls) {
	return compileXa(typecheckXa(desugarXa(implodeRules(decls))));
}

public Decl compileXa(Decl d) {
	return d;
 
/*	return innermost visit(d) {
		case [ss1*,matchCond(e,cs,a), s, ss2*] => ss1 + [matchCond(e,appendClauses(cs,[s,ss2]),a)]
		case [ss1*trans(lhs1,ops,rhs1) => matchCond((lhs1 | app(o, [it] + as, t) | app(o, as, t) <- ops), {<rhs1,[]>},nop())
		//case [ss1*,nop(), ss2*] => ss1 + ss2
	}
*/
}

public list[Decl] compileXa(tuple[list[Decl], set[Type]] dts) {
	ds = [compileXa(d) | d <- dts[0]];
	ts = dts[1];

	map[str,Decl] declared = ();

	for(d <- ds) switch(d) {
		case opDef(n,rt,args,body):
			declared += ("<n>(<[typeOf(a) | a <- args]>)" : d);
		case opDecl(n,rt,args):
			declared += ("<n>(<args>)" : d);
	}
	ds += makeImplicitOps(ds, declared); 

	rel[str,list[Expr],Decl] decls = {};
	rel[str,list[Type],str] rename = {};
	for(d <- ds) switch(d) {
		case opDef(n,rt,args,body): {
			treeArgs = [t | var(_,t) <- args, replaceLast(t, "\\*", "") in ts];
			if(size(treeArgs) > 0) {
				newn = replaceAll(n + strJoin(treeArgs, ""), "\\*", "List");
				rename += {<n, [t | var(_,t) <- args], newn>};
				decls += {<newn, args, opDef(newn,rt,args,body)>};
			}
			else
				decls += {<n,args,d>};
		}
		case opDecl(n,rt,args):
			;
		default:
			throw IllegalASTFormat("Illegal decl during compilation: <d>\n");
	}

	decls = renameApps(decls, rename);

	// combine multiple definitions of same op
	list[Decl] result = [];
	for(<n,args> <- decls<0,1>) {
		impls = toList(decls[n,args]);
		impl = (head(impls) | combine(it, c) | c <- tail(impls));
		impl.body = combine(impl.body, failure());
		result += impl;
	}
	return result;
}

public Decl combine(Decl d1, Decl d2) {
	if(opDef(_,_,_,body) := d1) {  // TODO: combine / rename arguments
		d1.body = combine(d1.body, d2.body);
		return d1;
	}
}

private &T renameApps(&T tree, rel[str, list[Type], str] rename) {
	return top-down-break visit(tree) {
		case app(n,as,t): {
				newn = rename[n, [typeOf(a) | a <- as]];
				if(size(newn) == 1)
					insert app(replaceAll(getOneFrom(newn), "\\*", "List"), renameApps(as, rename), t);
				else
					insert app(n, renameApps(as, rename), t);
		}
		case dqPattern(pat,t) => dqPattern(pat,t)  // break here to avoid traversing AsFix tree
		case sqPattern(pat,t) => sqPattern(pat,t)
	}
} 
private list[Decl] makeImplicitOps(&T tree, map[str, Decl] declared) {
	set[tuple[str, list[Type], Type, Decl]] implicit = {};
	visit(tree) {
		case app(n, as, rt): {
			list[str] ts = [typeOf(a) | a <- as];
			list[str] tsSingular = [replaceLast(t, "\\*", "") | t <- ts];
			if("<n>(<ts>)" notin declared && "<n>(<tsSingular>)" in declared) {
				// BUG: problems with lists of strings in relations / sets
				// println(<n, ts> in declared);
				// println(<n, tsSingular> in declared);
				// println(ts == tsSingular);
				//println(<n, ts>, " \<= ", <n, tsSingular>);
				implicit += <n, ts, rt, declared["<n>(<tsSingular>)"]>;
			}
		}
	}

	list[Decl] defs = [];
	for(<n, ps, rt, d> <- implicit) {
		switch(d) {
			case opDef(dn, drt, dps, dbody): {
					args = [var("<(dps[i].name)>[i_]", dps[i].sort) | i <- [0 .. size(dps)-1]];
					defs += opDef(n, rt, [var(dps[i].name,ps[i]) | i <- [0 .. size(dps)-1]], ret(foreach(var("i_","int"), "[0 .. size(<(dps[0].name)>)-1]", app(dn, args, drt))));
			}
			case opDecl(dn, drt, dps):
				defs += opDecl(n, rt, ps);
		}
	}
	println(defs);
	return defs;
}


public Clause combine(Clause c1, Clause c2) {
	switch(c1) {
		case condMatch(pat, expr, consq, altr):
			if(condMatch(pat, expr, consq2, altr2) := c2)
				return condMatch(pat, expr, combine(consq, consq2), combine(altr, altr2));
			else
				return condMatch(pat, expr, consq, combine(altr, c2));
		case condRel(name, args, consq, altr):
			if(condRel(name, args, consq2, altr2) := c2)
				return condRel(name, args, combine(consq, consq2), combine(altr, altr2));
			else
				return condRel(name, args, consq, combine(altr, c2));
		case nop():
			return c2;
		case ret(_):
			return c1;
		case block([c]):
			return combine(c, c2);
		case block(cs):
			if(block(cs2) := c2)
				return block([cs, cs2]);
			else
				return block([cs, c2]);
		case failure():
			return c1;
		default:
			throw IllegalASTFormat("Don\'t know what to do with <d>");
	}
}


public rel[Expr,list[Clause]] appendClauses(rel[Expr,list[Clause]] cs, list[Clause] ss) {
	return {<e,ts+ss> | <e,ts> <- cs};
}

public tuple[list[Decl], set[Type]] typecheckXa(tuple[list[Decl], set[Type]] dts) {
	if(<ds, ts> := dts) {
		Env env = {}; // {<"scope",["&T"], "&T">, <"resolve", ["&T"], "&T">, <"unscope", ["&T"], "&T">};
		return <typecheckXa(ds, env), ts>;
	}
}
	
public list[Decl] typecheckXa(list[Decl] ds, Env env) {
	for(i <- [0 .. size(ds)-1])
		<ds[i], env> = typecheckXa(ds[i], env);
	return ds;
}

public tuple[Decl,Env] typecheckXa(Decl d, Env env) {
	if(opDef(name, rt, args, body) := d) {
		bodyenv = env;
		newargs = [];
		count = 0;
		args = top-down-break visit(args) {
			case dqPattern(pat,s): {
				v = "<toLowerCase(s)><count>";
				count += 1;
				bodyenv += {<v, [], s>};
				body = condMatch(dqPattern(pat,s), var(v,s), body,nop());
				insert var(v,s);
			}
			case sqPattern(pat,s): {
				v = "<s><count>";
				count += 1;
				bodyenv += {<v, [], s>};
				body = condMatch(sqPattern(pat,s), var(v,s), body, nop());
				insert var(v,s);
			}
			case Var(v,t):
				bodyenv += {<v, [], s>};
		}
		<body, btype> = typecheckXa(body, bodyenv);

		return <opDef(name, btype, args, body), env + {<name, [typeOf(a) | a <- args], btype>}>;
	}
	else if(opDecl(name, rt, params) := d) {
		return <opDecl(name, rt, params), env + {<name, params, rt>}>;
	}
	else
		throw IllegalASTFormat("Don\'t know what to do with <d>");
}

public tuple[Clause,Type] typecheckXa(Clause c, Env env) {
	switch(c) {
		case condMatch(pat,expr,consq,altr): {
			<expr,etype> = typecheckXa(expr, env);
			<pat,ptype> = typecheckMatch(pat, etype, env);

			if(!typeEq(ptype, etype, ())[0]) 
				println("Pattern type doesn\'t match expression: <ptype> := <etype> in <pat> := <expr>");

			env1 = env + envOf(pat);
			<consq, ctype> = typecheckXa(consq, env1);
			<altr, atype> = typecheckXa(altr, env);

			if(ctype != atype && atype != "?")
				println("Inconsistent types in if clause: <ctype> vs. <atype>");

			return <condMatch(pat, expr, consq, altr), ctype>;
		}
		case condRel(name, args, consq, alt): {
			<args,targs> = unzip([typecheckXa(a,env) | a <- args]);
			<consq, ctype> = typecheckXa(consq, env);
			<altr, atype> = typecheckXa(altr, env);

			if(ctype != atype && atype != "?")
				println("Inconsistent types in if clause: <ctype> vs. <atype>");

			return <condRel(name, args, consq, altr), ctype>;
		}
		case relate(name, args): {
			<args,targs> = unzip([typecheckXa(a,env) | a <- args]);
			return <relate(name, args), "rel[<strJoin(targs, ",")>]">;
		}	
		case ret(expr): {
			<expr, etype> = typecheckXa(expr, env);
			return <ret(expr), etype>;
		}
		case block([cc]):
			return typecheckXa(cc, env);
		case block(cs): {
			Type t = "?";
			for(i <- [0 .. size(cs)-1])
				<cs[i], t> = typecheckXa(cs[i], env);
			return <block(cs), t>; 
		}
		case nop():
			return <nop(), "?">;
		case failure():
			return <failure(), "?">;
		default:
			throw IllegalASTFormat("Don\'t know what to do with clause <c>");
	}
}

public tuple[Expr,str] typecheckXa(Expr e, Env env) {
	switch(e) {
		case var(v,"?"): {
			t = lookup(v, env);
			return <var(v, t), t>;
		}
		case var(v,t):
			return <var(v, t), t>;
		case dqPattern(pat,t):
			return <dqPattern(pat,t), t>;
		case sqPattern(pat,t):
			return <sqPattern(pat,t), t>;
		case app(n,as,_): {
			<as,ts> = unzip([typecheckXa(a,env) | a <- as]);
			t = lookup(n, ts, env); 
			return <app(n, as, t), t>;
		}
		default:
			throw IllegalASTFormat("Don\'t know what to do with expr <e>");

	}
}

public tuple[Expr,str] typecheckMatch(Expr e, Type xt, Env env) {
	switch(e) {
		case var(v,"?"): {
			t = lookup(v, env, false);
			if(t != "?")
				return <var(v, t), t>;
			else
				return <var(v, xt), xt>;
		}
		case var(v,t):
			return <var(v, t), t>;
		case dqPattern(pat,t):
			return <dqPattern(pat,t), t>;
		case sqPattern(pat,t):
			return <sqPattern(pat,t), t>;
		case app(n,as,_): {
			paramsSet = {ps | <n,ps,xt> <- env, size(ps) == size(as)};
			t = xt;
			if(isEmpty(paramsSet))
				println("No matching constructor found in pattern decomposition: <n>(<as>) := (<xt>)");
			else if(size(paramsSet) > 1)
				println("Ambiguous constructor in pattern decomposition: <n>(<as>) := (<xt>)");
			else {
				params = getOneFrom(paramsSet);
				<as, ts> = unzip([typecheckMatch(as[i], params[i], env) | i <- [0 .. size(as)-1]]);
				t = lookup(n, ts, env);
			} 
			return <app(n, as, t), t>;
		}
		default:
			throw IllegalASTFormat("Don\'t know what to do with expr <e>");
	}
}

public Type lookup(str name, Env env) {
	return lookup(name, env, true);
}

public Type lookup(str name, Env env, bool err) {
	r = env[name,[]];
	if(size(r) > 1) {
		println("Ambiguous variable <name>");
		return getOneFrom(r);
	}
	else if(isEmpty(r)) {
		if(err)
			println("Unknown symbol <name>");
		return "?";
	}
	else
		return getOneFrom(r);
}
public Type lookup(str name, list[Type] args, Env env) {
	rs = env[name];
	if(isEmpty(rs)) {
		println("Unknown symbol <name>");
		return "?";
	}
	else for(<ps, t> <- rs) {
		if(ps == args)
			return t;
		else if(size(ps) == size(args)) {
			match = true;
			map[Type,Type] bindings = ();
			for(i <- [0 .. size(ps)-1]) {
				<eq,bindings> = typeEq(ps[i], args[i], bindings);
				match = match && eq;
			}
			if(match)
				return replace(t, bindings);
		}
	}
	println("No matching operator found for <name>(<strJoin(args,",")>)");
	return "?";
}

public Type replace(Type t, map[Type, Type] bindings) {
	if(t in bindings)
		return bindings[t];
	else
		return t;
}

public tuple[bool,map[str,str]] typeEq(Type t1, Type t2, map[Type,Type] bindings) {
	t1 = replace(t1, bindings);
	t2 = replace(t2, bindings);
	if(startsWith(t1, "&")) {
		bindings[t1] = t2;
		t1 = t2;
	}
	return <t1 == t2, bindings>;
}

public Env envOf(value e) {
	Env env = {};
	visit(e) {
		case MXaRascalVar v: env += {<"<astArgOf(v,1)>", [], "<astArgOf(v,0)>">};
		case MXaRascalListVar v: env += {<"<astArgOf(v,1)>", [], "<astArgOf(v,0)>">};
		case var(v, t): env += {<v, [], t>};
	}
	return env;
}

	

/*
import MetaXa;
import XaRules;
import RascalBackend;
import IO;
ruleDefs = compile(|file:///home/anya/workspace/MetaXa/src/Magnolia.mxa|);
println(pp(typecheckXa(desugarXa(implodeRules(ruleDefs)))));



./XaRules.rsc:292,63: IllegalASTFormat("Don't know what to do with clause 


*/
