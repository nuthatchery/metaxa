module syn::Analysis

import syn::MetaXa;
import ParseTree;
import IO;


data NameEnv
	= NameEnv(str scopeName, map[str, Id] decls)
	;
	
data DefEnv
	= DefEnv(rel[Id, Def] defs)
	;

data Def
	= Def(Type typ, Symbol sort)
	;
	
data Type 
	= Type(str name)
	| Type(str name, list[Type] params)
	;

public DefEnv checkModule((Module)`<Decl* decls>`) {
	dEnv = DefEnv({});
	nEnv = NameEnv("", ());
	
	for(d <- decls) {
		dEnv = checkDecl(d, nEnv, dEnv);
	}
	
	return dEnv;
	
}

public DefEnv checkDecl((Decl)`sort <Id name> { <Decl* decls> }`, NameEnv nEnv, DefEnv dEnv) {
  
}

public void printSyntaxDefs(Tree m) {
	//for(/SyntaxBody b <- m) {
	visit(m) {
		case (SyntaxBody)`{ <SyntaxToken* toks> }`: {
			for(t <- toks) {
				print("<getLabel(t)>[");
				for(u <- t.args)
					print("(<u>)");
				print("]");
			}
			println();
		}
		
		case TypeExpr t:                              
			println( "<checkType( t, NameEnv("", ()), DefEnv({}) )>" );
	}
	//}
}

public str getLabel(Tree t) {
	try {
		if(label(s, _) := t.prod.def)
			return s;
		else
			return "";
	}
	catch NoSuchField(_):
		println([<t>]);
}

tuple[Type, DefEnv] checkType(TypeExpr t, NameEnv nEnv, DefEnv dEnv) {
	switch(t) {
		case (TypeExpr)`<Id n>`:     
			return <Type(unparse(n)), dEnv>; 
		case (TypeExpr)`<Id n>[<{TypeExpr ","}* params>]`: {
			list[Type] ps = [];
			for(p <- params) {
				<c, env> = checkType(p, nEnv, dEnv);
				ps += c;
			}
			return <Type(unparse(n), ps), dEnv>;
		}
	}
}
