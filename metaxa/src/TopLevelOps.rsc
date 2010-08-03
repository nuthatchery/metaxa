module TopLevelOps
import MagnoliaAST;
import IO;
import MagnoliaCompileLib;
import Set;
import List;
import Node;

public AST applyImpl(AST lhs, AST rhs, AST orig) {
	set[ErrorMark] marks = {};
	
	try {
		unDefs = sigDiff(onOf(rhs), definesOf(lhs));
		for(d <- unDefs) {
			if(isIn(locOf(d), locOf(orig)))
				marks = addMark(marks, "Unsatisfied dependency in composition", d);
			else
				marks = addMark(marks, "Unsatisfied dependency <nameOf(d)> (from <locInfo(locOf(d), locOf(orig))>) in composition", orig);
			
		}
		
		// inline the lhs defs that appear in the on-clause of rhs
		<rhsInlined, errs> = inline(rhs, sigIsect(definesOf(lhs), onOf(rhs)));
		marks += errs;
		//if(size(unDefs) > 0)
		//	return addMarks(marks, orig);
	
		defPart = decls(sigUnion(definesOf(rhsInlined), definesOf(lhs)));
		
		AST result;
		switch(lhs) {
			case OnDefines(onPart, _):
				result = OnDefines(onPart, defPart);
			case External(ExternalExtendsOnDefines(language,name,
								extends,onPart, _)):
				result = External(ExternalExtendsOnDefines(language, name, 
								extends, onPart, defPart));
			default: {
				marks = addMark(marks, "Don\'t know how to apply implementation to <getName(lhs)>", lhsOf(orig));
				result = At(lhs, rhs);
			}
		}
		return result[@mark=marks];
	}
	catch InternalError(msg, ast): {
		if(locOf(ast) != unknownLoc)
			return addMark(orig, msg, "error", [locOf(ast)]);
		else
			return addMark(orig, msg);
	}
}


public AST makeExternal(AST language, AST name, AST tree) {
	if(DeclBody(seq(ds)) := tree) {
		for(i <- domain(ds)) {
			switch(ds[i]) {
				case Define(mods,def,attrs,body):
					ds[i] = makeExternal(language, name, mods,def,attrs);
				default:
					throw InternalError("Unknown declaration <getName(ds[i])>", ds[i]);
			}
			println(ds[i]);
		}
		return DeclBody(seq(ds));
	}
	else
		throw InternalError("Top-level expression not fully flattened: <getName(tree)>", tree);
		
}

private AST makeExternal(AST language, AST name, AST mods, AST def, AST attrs) {
	if(seq(as) := attrs)
		attrs = seq([a | a <- as, a != External()]);
	switch(def) {
		case TypeClause(n):
			return Define(mods, def, attrs, BodyS(ExternalType(language, qualify(name, n))));
		case FunClause(n,ps,t):
			return Define(mods, def, attrs, BodyS(Apply(ExternalFun(language, qualify(name, n)), seq(paramsToArgs(ps)))));
		case ProcClause(n,ps):
			return Define(mods, def, attrs, BodyS(Call(ExternalProc(language, qualify(name, n)), seq(paramsToArgs(ps)))));
		default:
			throw InternalError("Unknown declaration clause <getName(def)>", def);
	}
}

public set[AST] definesOf(AST expr) {
	result = Nop();
	switch(expr) {
		case OnDefines(_, defPart): 
			result = defPart;
		case External(ExternalExtendsOnDefines(_,_,_,_,defPart)): 
			result = defPart;
		case DeclBody(seq(ds)): 
			result = defPart;
	}
	
	if(DeclBody(seq(ds)) := result)
		return toSet(ds);
	else if(Nop() := result)
		throw InternalError("Don\'t know how to get defines-part from <getName(expr)>", expr);
	else
		throw InternalError("Top-level expression not fully flattened: <getName(expr)>", expr);
}

public set[AST] onOf(AST expr) {
	result = Nop();
	switch(expr) {
		case OnDefines(onPart, _): 
			result = onPart;
		case External(ExternalExtendsOnDefines(_,_,_,onPart,_)):
			result = onPart;
	}

	if(DeclBody(seq(ds)) := result)
		return toSet(ds);
	else if(Nop() := result)
		throw InternalError("Don\'t know how to get on-part from <getName(expr)>", expr);
	else
		throw InternalError("Top-level expression not fully flattened: <getName(expr)>", expr);
}

public set[AST] fullOf(AST expr) {
	return sigUnion(definesOf(expr), onOf(expr));
}

public set[AST] signatureOf(AST expr) {
	result = expr;
	switch(expr) {
		case Define(mod,dcl,sub,_):
			result = {Define(mod, dcl, sub, EmptyBodyS())};
		case DeclBody(seq(ds)):
			result = ({} | it + signatureOf(d) | d <- ds);
		default:
			throw InternalError("Don\'t know how to extract signature from <getName(expr)>", expr);
	}
	
	return preserveAnnos(result, expr);
}

public AST canonical(AST decl) {
	result = decl;
	switch(decl) {
		case Define(_,dcl,_,_):
			result = Define(Nop(), dcl, Nop(), EmptyBodyS());
		default:
			throw InternalError("Don\'t know how to canonicalize <getName(decl)>", decl);
	}
	
	return preserveAnnos(result, decl);
}

public tuple[AST, AST] lhsAndRhsOf(AST expr) {
	switch(expr) {
		case Models(a, b):
			return <a, b>;
		case WithModels(a, w, b):
			return <a, b>;
		case Morphed(a, b):
			return <a, b>;
		case Protected(a, b):
			return <a, b>;
		case OnDefines(a, b):
			return <a, b>;
		case At(a, b):
			return <a, b>;
		case AtAt(a, b):
			return <a, b>;
		case Plus(a, b):
			return <a, b>;
		case PlusPlus(a, b):
			return <a, b>;
		case Times(a, b):
			return <a, b>;
		case Times(a, b):
			return <a, b>;
		case DataInvariant(a, b):
			return <a, b>;
		case Quotient(a, b):
			return <a, b>;
		default:
			throw InternalError("Unknown top-level expr in lhsAndRhsOf(): <getName(expr)>", expr);
	}
}

public AST lhsOf(AST expr) {
	return lhsAndRhsOf(expr)[0];
}

public AST rhsOf(AST expr) {
	return lhsAndRhsOf(expr)[1];
}

@doc{Set difference, with respect to the canonical representation of the
declarations in the sets.}
public set[AST] sigDiff(set[AST] lhs, set[AST] rhs) {
	rhs = {canonical(d) | d <- rhs};
	return {d | d <- lhs, canonical(d) notin rhs};
}

@doc{Set union, with respect to the canonical representation of the
declarations in the sets. Overlapping elements are taken from the lhs set.}
public set[AST] sigUnion(set[AST] lhs, set[AST] rhs) {
	lhs2 = {canonical(d) | d <- lhs};
	return lhs + {d | d <- rhs, canonical(d) notin lhs};
}

@doc{Set intersection, with respect to the canonical representation of the
declarations in the sets. Elements are taken from the lhs set.}
public set[AST] sigIsect(set[AST] lhs, set[AST] rhs) {
	rhs = {canonical(d) | d <- rhs};
	return {d | d <- lhs, canonical(d) in rhs};
}

public AST decls(set[AST] ds) {
	return DeclBody(seq(toList(ds)));
}

