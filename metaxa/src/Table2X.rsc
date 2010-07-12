module Table2X
import ATermIO;
import IO;
import XaLib;
import XaTree;
import String;
import Node;

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
	map[str,str] ppTable = ();
	set[str] deprecated = {};
	list[str] astDefs = [];
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
					
					prods += p;
					if(getCons(attrs) != "") {
						<consname, concrete> = getPP(lhs, rhs, attrs);
						if(consname in ppTable && ppTable[consname] != concrete) {
							if(consname in deprecated) {
								if(!hasAttr("deprecated", attrs)) {
									ppTable[consname] = concrete;
									deprecated -= consname;
								}
								else
									println("Overlapping deprecated syntax for <consname>: <concrete>");
							}
							else if(!hasAttr("deprecated", attrs))
								println("Overlapping syntax for <consname>: <concrete>");
						}
						else {
							ppTable[consname] = concrete;
							if(hasAttr("deprecated", attrs))
								deprecated += consname;
						}
							
								
						astDefs += getADT(lhs, rhs, attrs);
					}
					//else
					//	println(getInj(lhs, rhs, attrs));
				}
			}
		}
	}
	
	rascalFile = location;
	rascalFile.path = replaceLast(location.path, "/[^/]*", "/<language>AST.rsc");
	writeFile(rascalFile, "module <language>AST\n\n<strJoin(astDefs,"\n")>\n\n");
	javaPP = "";
	for(consname <- ppTable)
		javaPP += "\t\ttbl.put(\"<consname>\", <ppTable[consname]>);\n";
	javaFile = location;
	javaFile.path = replaceLast(location.path, "/[^/]*", "/<language>SkinTable.java");
	writeFile(javaFile, "package org.magnolialang.xatree;\n\nimport java.util.HashMap;\nimport java.util.Map;\nimport org.eclipse.imp.pdb.facts.IList;\n"
		+ "import static org.magnolialang.xatree.XaTreeFactory.*;\n\n"
		+ "class <language>SkinTable {\n\tpublic static Map\<String,IList\> getMap() {\n"
		+ "\t\tMap\<String,IList\> tbl = new HashMap\<String,IList\>();\n\n<javaPP>\t\treturn tbl;\n\t}\n}\n");
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

public tuple[str,str] getPP(list[value] lhs, value rhs, list[value] attrs) {
	list[XaToken] pp = [];
	int chld = 0;
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit):
				pp += token("<lit>");
			case "cf"("opt"("layout"())):
				pp += space(" ");
			case "cf"("iter-sep"(sort,"lit"(lit))): {
				pp += sep(child(chld), "<lit>");
				chld = chld + 1;
			}
			case "cf"("iter-star-sep"(sort,"lit"(lit))): {
				pp += sep(child(chld), "<lit>");
				chld = chld + 1;
			}
			default: {
				pp += child(chld);
				chld = chld + 1;
			}
		}
	}
	
	return <"<getCons(attrs)>/<chld>:<pTblSortName(rhs)>", "vf.list(<strJoin(["<p>" | p <- pp], ", ")>)">;
}

public str getADT(list[value] lhs, value rhs, list[value] attrs) {
	list[str] defs = [];
	int chld = 0;
	
	for(sym <- lhs) {
		switch(sym) {
			case "lit"(lit):
				;
			case "cf"("opt"("layout"())):
				;
			case sort: {
				defs += "AST"; //rascalSortName(sort);
				chld = chld + 1;
			}
		}
	}
	
	return left("data AST = <getCons(attrs)>(<strJoin(defs, ", ")>);", 70) + "  // <getSyn(lhs, rhs, attrs)>";
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
			case sort: {
				syn += pTblSortName(sort);
			}
		}
	}
	
	return "<pTblSortName(rhs)> ::= <strJoin(syn, "")>"; 

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
			default:
				println("Whoops!");
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

	data AST = MagnoliaTree(AST, AST);                                     // Program ::= ModuleHead TopDecl*
	data AST = ModuleHead(AST, AST);                                       // ModuleHead ::= "module" Name ModuleClause* ";"
	data AST = Name(AST);                                                  // Identifier ::= ID
	data AST = Name(AST);                                                  // Name ::= ID
	data AST = QName(AST, AST);                                            // Name ::= Name "." ID
	data AST = Type(AST);                                                  // Type ::= Name
	data AST = NilType();                                                  // Type ::= "(" ")"
	data AST = AltType(AST, AST);                                          // Type ::= Type "|" Type
	data AST = ProdType(AST, AST);                                         // ProdType ::= Type "," ProdType
	data AST = Dummy(AST);                                                 // Type ::= "(" ProdType ")"
	data AST = Struct(AST);                                                // Type ::= "struct" DeclBody
	data AST = Int(AST);                                                   // Literal ::= DecNumeral
	data AST = Real(AST);                                                  // Literal ::= FloatNumeral
	data AST = String(AST);                                                // StringLiteral ::= """ String """
	data AST = Block(AST);                                                 // BlockStat ::= "{" Stat* "}"
	data AST = Nop();                                                      // Stat ::= ";"
	data AST = If(AST, AST, AST);                                          // Stat ::= "if" Expr "then" Stat* "else" Stat* "end"
	data AST = While(AST, AST);                                            // Stat ::= "while" Expr "do" Stat* "end"
	data AST = Open(AST, AST);                                             // Stat ::= "open" {Identifier ","}* "in" Stat* "end"
	data AST = For(AST, AST, AST);                                         // Stat ::= "for" Identifier "in" Expr "do" Stat* "end"
	data AST = Call(AST, AST);                                             // Stat ::= "call" ProcName "(" {Expr ","}* ")" ";"
	data AST = Proc(AST);                                                  // ProcName ::= Identifier
	data AST = Yield(AST);                                                 // Stat ::= "yield" Expr ";"
	data AST = Break();                                                    // Stat ::= "break" ";"
	data AST = Return();                                                   // Stat ::= "return" ";"
	data AST = Return(AST);                                                // Stat ::= "return" Expr ";"
	data AST = Assign(AST, AST);                                           // Stat ::= Identifier "=" Expr ";"
	data AST = Let(AST, AST);                                              // Stat ::= "let" LetClause* "in" Stat* "end"
	data AST = VarDef(AST, AST, AST);                                      // LetClause ::= "var" Identifier ":" Type "=" Expr ";"
	data AST = Assert(AST, AST);                                           // Stat ::= "assert" Expr AssertClause* ";"
	data AST = By(AST);                                                    // AssertClause ::= "by" Expr
	data AST = By(AST);                                                    // AssertClause ::= "by" "simplify" Expr
	data AST = QED();                                                      // AssertClause ::= "qed"
	data AST = Undefined();                                                // Expr ::= "_"
	data AST = Var(AST, AST);                                              // Var ::= Name ":" Type
	data AST = Literal(AST);                                               // Expr ::= Literal
	data AST = Tuple(AST);                                                 // Expr ::= "(" {Expr ","}* ")"
	data AST = TypeExpr(AST);                                              // Expr ::= ":" Type
	data AST = Struct(AST, AST);                                           // Expr ::= Type "$" "{" {InitSpec ","}* "}"
	data AST = Struct(AST, AST);                                           // Expr ::= Type "{" {InitSpec ","}* "}"
	data AST = Field(AST, AST);                                            // InitSpec ::= Identifier ":=" Expr
	data AST = Apply(AST, AST);                                            // Expr ::= Fun "(" {Expr ","}* ")"
	data AST = Fun(AST);                                                   // Fun ::= FunName
	data AST = IfThenElseExpr(AST, AST, AST);                              // Expr ::= "if" Expr "then" Expr "else" Expr "end"
	data AST = BlockExpr(AST);                                             // Expr ::= "{" Stat* "}"
	data AST = ListCons(AST);                                              // Expr ::= "[" {Expr ","}* "]"
	data AST = ListCons(AST, AST);                                         // Expr ::= "[" {Expr ","}* "|" Expr "]"
	data AST = NumRep(AST, AST);                                           // DataRep ::= DecNumeral ".." DecNumeral ";"
	data AST = AliasType(AST);                                             // DataRep ::= "type" Type ";"
	data AST = StructRep(AST);                                             // DataRep ::= "struct" "{" Decl* "}"
	data AST = UnionRep(AST);                                              // DataRep ::= "union" "{" Decl* "}"
	data AST = TermRep(AST);                                               // DataRep ::= {ConsSpec ","}* ";"
	data AST = TermCons0(AST);                                             // ConsSpec ::= Identifier
	data AST = TermCons(AST, AST);                                         // ConsSpec ::= Identifier "(" {DataRep ","}* ")"
	data AST = DefDecl(AST, AST, AST, AST);                                // BraceDecl ::= Modifier* StatDeclarative SubClause* BlockStat
	data AST = DefDecl(AST, AST, AST, AST);                                // SemiDecl ::= Modifier* ExprDeclarative SubClause* "=" Expr ";"
	data AST = DefDecl(AST, AST, AST, AST);                                // Decl ::= Modifier* TypeDeclarative SubClause* "=" Type ";"
	data AST = DefDecl(AST, AST, AST, AST);                                // BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
	data AST = DeclBody(AST);                                              // DeclBody ::= "{" Decl* "}"
	data AST = ProcClause(AST, AST);                                       // ProcClause ::= "procedure" ProcIdentifier ProcedureParamList
	data AST = Assign();                                                   // ProcIdentifier ::= "_=_"
	data AST = FunClause(AST, AST, AST);                                   // FunClause ::= "function" FunIdentifier FunctionParamList ":" Type
	data AST = PredClause(AST, AST);                                       // PredClause ::= "predicate" FunIdentifier FunctionParamList
	data AST = AxiomClause(AST, AST);                                      // AxiomClause ::= "axiom" Identifier FunctionParamList
	data AST = AxiomClause(AST, AST);                                      // AxiomClause ::= "theorem" Identifier FunctionParamList
	data AST = AxiomClause(AST, AST);                                      // AxiomClause ::= "proof" Identifier FunctionParamList
	data AST = Dummy(AST);                                                 // FunctionParamList ::= "(" {FunctionParam ","}* ")"
	data AST = Dummy(AST);                                                 // ProcedureParamList ::= "(" {ProcedureParam ","}* ")"
	data AST = Param(AST, AST);                                            // FunctionParam ::= VarIdentifier ":" Type
	data AST = Param(AST, AST, AST);                                       // ProcedureParam ::= ParamMode VarIdentifier ":" Type
	data AST = Obs();                                                      // ParamMode ::= "obs"
	data AST = Upd();                                                      // ParamMode ::= "upd"
	data AST = Out();                                                      // ParamMode ::= "out"
	data AST = Exp();                                                      // ParamMode ::= "exp"
	data AST = Giv();                                                      // ParamMode ::= "giv"
	data AST = Del();                                                      // ParamMode ::= "del"
	data AST = Nrm();                                                      // ParamMode ::= "nrm"
	data AST = VarClause(AST, AST);                                        // VarClause ::= "var" VarIdentifier ":" Type
	data AST = TypeClause(AST);                                            // TypeClause ::= "type" TypeIdentifier
	data AST = ConceptClause(AST, AST, AST, AST);                          // Decl ::= Modifier* "concept" Identifier SubClause* "=" ConceptExpr
	data AST = ImplClause(AST, AST, AST, AST);                             // Decl ::= Modifier* "implementation" Identifier SubClause* "=" ImplExpr
	data AST = Library(AST, AST, AST, AST);                                // Decl ::= Modifier* "library" Identifier SubClause* "=" ImplExpr
	data AST = ConceptClause(AST, AST, AST, AST);                          // Decl ::= Modifier* "concept" Identifier SubClause* DeclBody
	data AST = ImplClause(AST, AST, AST, AST);                             // Decl ::= Modifier* "implementation" Identifier SubClause* ImplExpr
	data AST = Attrs(AST);                                                 // AttrClause ::= "[" {Attribute ","}* "]"
	data AST = Attr(AST, AST);                                             // Attribute ::= Name "(" {Expr ","}* ")"
	data AST = Guard(AST);                                                 // GuardClause ::= "guard" Expr
	data AST = Requires(AST);                                              // Decl ::= "requires" {ConceptExpr ","}+ ";"
	data AST = Partition(AST, AST);                                        // Decl ::= "partition" TypeName "by" {FunClause ","}* ";"
	data AST = Generate(AST, AST);                                         // Decl ::= "generate" TypeName "by" {FunClause ","}* ";"
	data AST = Generate(AST, AST);                                         // Decl ::= "generate" TypeName "by" {Expr ","}* ";"
	data AST = Free(AST, AST);                                             // Decl ::= "free" TypeName "by" {FunClause ","}* ";"
	data AST = ModelsImpl(AST, AST, AST, AST, AST);                        // Decl ::= "satisfaction" Identifier SubClause* "=" ConceptExpr WithClause? "models" ConceptExpr
	data AST = ModelsImpl(AST, AST, AST, AST, AST);                        // Decl ::= "satisfaction" Identifier SubClause* "=" "on" ConceptExpr WithClause? "models" ConceptExpr
	data AST = WithClause(AST);                                            // WithClause ::= "with" ImplExpr
	data AST = Concept(AST);                                               // ConceptExpr ::= Name
	data AST = ConceptBody(AST);                                           // ConceptExpr ::= DeclBody
	data AST = SignatureOf(AST);                                           // ConceptExpr ::= "signature" ConceptExpr
	data AST = MorphedConcept(AST, AST);                                   // ConceptExpr ::= ConceptExpr Morphism
	data AST = Impl(AST);                                                  // ImplExpr ::= Name
	data AST = ImplMorph(AST, AST);                                        // ImplExpr ::= ImplExpr Morphism
	data AST = ImplBody(AST);                                              // ImplExpr ::= DeclBody
	data AST = ExternalLib(AST, AST, AST);                                 // ImplExpr ::= "external" Identifier Name "defines" ImplExpr
	data AST = ExternalLib(AST, AST, AST, AST);                            // ImplExpr ::= "external" Identifier Name "on" ConceptExpr "defines" ImplExpr
	data AST = OnDefines(AST, AST);                                        // ImplExpr ::= "on" ConceptExpr "defines" ImplExpr
	data AST = Morphism(AST);                                              // Morphism ::= "morphism" MorphClause
	data AST = Morphism(AST);                                              // SubClause ::= "morphism" MorphClause
	data AST = ImplMorphism(AST);                                          // MorphClause ::= "{" MorphClause* "}"
	data AST = ImplMorphism(AST);                                          // MorphClause ::= "{" (MorphClause ";")* "}"
	data AST = Morphisms(AST);                                             // Morphism ::= "[" {MorphClause ","}* "]"
	data AST = Rename(AST, AST);                                           // MorphClause ::= Name "=>" Name
	data AST = Morphism(AST, AST);                                         // MorphClause ::= TypeClause "=" Type
	data AST = Morphism(AST, AST);                                         // MorphClause ::= FunClause "=" Expr
	data AST = Morphism(AST, AST);                                         // MorphClause ::= PredClause "=" Expr
	data AST = Protect(AST, AST);                                          // Morphism ::= "protect" FunClause "guard" Expr
	data AST = Protect(AST, AST);                                          // MorphClause ::= "protect" FunClause "guard" Expr
	data AST = Protect(AST, AST);                                          // SubClause ::= "protect" FunClause "guard" Expr
	data AST = Protect(AST, AST);                                          // Morphism ::= "protect" FunClause "by" Expr
	data AST = Protect(AST, AST);                                          // MorphClause ::= "protect" FunClause "by" Expr
	data AST = Protect(AST, AST);                                          // SubClause ::= "protect" FunClause "by" Expr
	data AST = At(AST, AST);                                               // ImplExpr ::= ImplExpr "@" ImplExpr
	data AST = AtAt(AST, AST);                                             // ImplExpr ::= ImplExpr "@@" ImplExpr
	data AST = Star(AST, AST);                                             // ImplExpr ::= ImplExpr "*" ImplExpr
	data AST = StarStar(AST, AST);                                         // ImplExpr ::= ImplExpr "**" ImplExpr
	data AST = Plus(AST, AST);                                             // ImplExpr ::= ImplExpr "+" ImplExpr
	data AST = PlusPlus(AST, AST);                                         // ImplExpr ::= ImplExpr "++" ImplExpr
	data AST = Times(AST, AST);                                            // ImplExpr ::= ImplExpr "times" ImplExpr
	data AST = At(AST, AST);                                               // ConceptExpr ::= ConceptExpr "@" ConceptExpr
	data AST = AtAt(AST, AST);                                             // ConceptExpr ::= ConceptExpr "@@" ConceptExpr
	data AST = Star(AST, AST);                                             // ConceptExpr ::= ConceptExpr "*" ConceptExpr
	data AST = StarStar(AST, AST);                                         // ConceptExpr ::= ConceptExpr "**" ConceptExpr
	data AST = Plus(AST, AST);                                             // ConceptExpr ::= ConceptExpr "+" ConceptExpr
	data AST = PlusPlus(AST, AST);                                         // ConceptExpr ::= ConceptExpr "++" ConceptExpr
	data AST = Times(AST, AST);                                            // ConceptExpr ::= ConceptExpr "times" ConceptExpr
	data AST = Hex(AST);                                                   // Literal ::= HexNumeral
	data AST = Bin(AST);                                                   // Literal ::= BinNumeral
	data AST = Oct(AST);                                                   // Literal ::= OctNumeral
	data AST = BaseFor(AST, AST, AST);                                     // Stat ::= "for" Identifier "in" Expr Stat
	data AST = BasePrintLn(AST);                                           // Stat ::= "print" {Expr ","}* ";"
	data AST = BasePrint(AST);                                             // Stat ::= "print" {Expr ","}* "," ";"
	data AST = BaseVarDefTI(AST, AST, AST);                                // Stat ::= "var" Identifier ":" Type "=" Expr ";"
	data AST = BaseVarDefI(AST, AST);                                      // Stat ::= "var" Identifier "=" Expr ";"
	data AST = BaseVarDefI(AST);                                           // Stat ::= "var" Identifier ";"
	data AST = BaseVarDefT(AST, AST);                                      // Stat ::= "var" Identifier ":" Type ";"
	data AST = DotOp(AST, AST);                                            // Expr ::= Expr "." Identifier
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr MULOP Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr ADDOP Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr SHFOP Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr RNGOP Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr CMPOP Expr
	data AST = In(AST, AST);                                               // Expr ::= Expr "in" Expr
	data AST = NotIn(AST, AST);                                            // Expr ::= Expr "not" "in" Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr EQUOP Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr BTAND Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr BTXOR Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr BITOR Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr SUBST Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr LGAND Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr LOGOR Expr
	data AST = BinOp(AST, AST, AST);                                       // Expr ::= Expr LGIMP Expr
	data AST = Index(AST, AST);                                            // Expr ::= Expr "[" {Expr ","}* "]"
	data AST = PreOp(AST, AST);                                            // Expr ::= NEGOP Expr
	data AST = PreOp(AST, AST);                                            // Expr ::= BTNOT Expr
	data AST = PreOp(AST, AST);                                            // Expr ::= LGNOT Expr
	data AST = NoDefDecl(AST, AST);                                        // Decl ::= StatDeclarative SubClause* ";"
	data AST = NoDefDecl(AST, AST);                                        // Decl ::= ExprDeclarative SubClause* ";"
	data AST = NoDefDecl(AST, AST);                                        // Decl ::= TypeDeclarative SubClause* ";"
	data AST = FunClause(AST, AST);                                        // FunClause ::= "define" FunName ":" Type
	data AST = DataInvariant(AST);                                         // FunClause ::= "dataInvariant" FunctionParamList
	data AST = DataInvariant(AST);                                         // ProcClause ::= "dataInvariant" ProcedureParamList
	data AST = Congruence(AST);                                            // FunClause ::= "congruence" FunctionParamList
	data AST = Congruence(AST);                                            // ProcClause ::= "congruence" ProcedureParamList
	data AST = AnonParam(AST);                                             // FunctionParam ::= Type
	data AST = ObsParam(AST, AST);                                         // ProcedureParam ::= Identifier ":" Type
	data AST = AnonParam(AST, AST);                                        // ProcedureParam ::= ParamMode ":" Type
	data AST = Attr(AST);                                                  // Attribute ::= Identifier
	data AST = Opens(AST);                                                 // SubClause ::= "opens" "(" {Identifier ","}* ")"
	data AST = Default(AST);                                               // SubClause ::= "default" "(" {Identifier ","}* ")"
	data AST = SimpleModule();                                             // ModuleHead ::= 
	data AST = Language(AST);                                              // ModuleClause ::= "language" {Name ","}*
	data AST = Imports(AST);                                               // ModuleClause ::= "imports" {ImportClause ","}*
	data AST = Requires(AST);                                              // ModuleClause ::= "requires" {RequiresClause ","}*
	data AST = CompilePragma(AST, AST);                                    // ModuleClause ::= "compile" """ String """ "{" CompileClause* "}"
	data AST = Pragma(AST, AST);                                           // CompileClause ::= ID "(" {QuotedString ","}* ")" ";"
	data AST = Dummy(AST);                                                 // QuotedString ::= """ String """
	data AST = ImportRequires(AST, AST);                                   // RequiresClause ::= Identifier "<" {ConceptClauseArgument ","}* ">"
	data AST = ImportAll(AST);                                             // ImportClause ::= Name
	data AST = Unresolved(AST);                                            // ConceptClauseArgument ::= Type
	data AST = ImportModule(AST);                                          // ImportClause ::= "module" Name
	data AST = ImportRename(AST, AST);                                     // ImportClause ::= "module" Name "=" Name
	data AST = Nop();                                                      // Decl ::= ";"
	data AST = UserSyntax0(AST, AST, AST);                                 // Stat ::= Expr "." Identifier "=" Expr ";"
	data AST = UserSyntax1(AST, AST, AST);                                 // Stat ::= Identifier "[" {Expr ","}+ "]" "=" Expr ";"
	data AST = UserSyntax2(AST);                                           // Expr ::= Name
	data AST = UserSyntax3(AST, AST);                                      // Stat ::= "if" Expr "then" Stat* "end"
	data AST = UserSyntax4(AST, AST);                                      // Stat ::= "if" "(" Expr ")" "{" Stat* "}"
	data AST = UserSyntax5(AST, AST, AST);                                 // Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"
