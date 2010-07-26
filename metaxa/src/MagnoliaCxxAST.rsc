module MagnoliaCxxAST

data AST = Name(AST);                                                   // Identifier ::= ID
data AST = Name(AST);                                                   // Name ::= ID
data AST = QName(AST, AST);                                             // Name ::= Name "::" ID
data AST = Type(AST);                                                   // Type ::= Name
data AST = NilType();                                                   // Type ::= "void"
data AST = Struct(AST);                                                 // Type ::= "struct" DeclBody
data AST = Int(AST);                                                    // Literal ::= DecNumeral
data AST = Real(AST);                                                   // Literal ::= FloatNumeral
data AST = String(AST);                                                 // StringLiteral ::= """ String """
data AST = Hex(AST);                                                    // Literal ::= HexNumeral
data AST = Bin(AST);                                                    // Literal ::= BinNumeral
data AST = Oct(AST);                                                    // Literal ::= OctNumeral
data AST = Block(AST);                                                  // BlockStat ::= "{" Stat* "}"
data AST = Nop();                                                       // Stat ::= ";"
data AST = If(AST, AST, AST);                                           // Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"
data AST = While(AST, AST);                                             // Stat ::= "while" "(" Expr ")" "{" Stat* "}"
data AST = CxxFor(AST, AST, AST);                                       // Stat ::= "for" "(" Decl Expr ";" Expr ")"
data AST = Call(AST, AST);                                              // Stat ::= Proc "(" {Expr ","}* ")" ";"
data AST = Proc(AST);                                                   // Proc ::= ProcName
data AST = Yield(AST);                                                  // Stat ::= "return" Expr ";"
data AST = Break();                                                     // Stat ::= "break" ";"
data AST = Return();                                                    // Stat ::= "return" ";"
data AST = Assign(AST, AST);                                            // Stat ::= Identifier "=" Expr ";"
data AST = Let(AST, AST);                                               // Stat ::= "{" LetClause* Stat* "}"
data AST = VarDef(AST, AST, AST);                                       // LetClause ::= Type Identifier "=" Expr ";"
data AST = Assert(AST, AST);                                            // Stat ::= "assert" "(" Expr AssertClause* ")" ";"
data AST = By(AST);                                                     // AssertClause ::= "," Expr
data AST = By(AST);                                                     // AssertClause ::= "," "simplify" "(" Expr ")"
data AST = QED();                                                       // AssertClause ::= "qed" "(" ")"
data AST = Undefined();                                                 // Expr ::= "_"
data AST = Var(AST);                                                    // Var ::= Name
data AST = Literal(AST);                                                // Expr ::= Literal
data AST = Apply(AST, AST);                                             // Expr ::= Fun "(" {Expr ","}* ")"
data AST = Fun(AST);                                                    // Fun ::= FunName
data AST = IfThenElseExpr(AST, AST, AST);                               // Expr ::= Expr "?" Expr ":" Expr
data AST = ListCons(AST);                                               // Expr ::= "list" "(" {Expr ","}* ")"
data AST = ListCons(AST, AST);                                          // Expr ::= "list" "(" {Expr ","}* "," Expr ")"
data AST = DefDecl(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* StatDeclarative SubClause* BlockStat
data AST = DefDecl(AST, AST, AST, AST);                                 // SemiDecl ::= Modifier* ExprDeclarative SubClause* "{" "return" Expr ";" "}"
data AST = DefDecl(AST, AST, AST, AST);                                 // Decl ::= Modifier* "typedef" TypeDeclarative SubClause* "=" Type ";"
data AST = DefDecl(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* StatDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* ExprDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* "struct" TypeDeclarative SubClause* ";"
data AST = DeclBody(AST);                                               // DeclBody ::= "{" Decl* "}"
data AST = ProcClause(AST, AST);                                        // ProcClause ::= "void" ProcIdentifier ProcedureParamList
data AST = Assign();                                                    // ProcIdentifier ::= "operator" "="
data AST = FunClause(AST, AST, AST);                                    // FunClause ::= Type FunIdentifier FunctionParamList
data AST = Guard(AST);                                                  // GuardClause ::= "guard" Expr
data AST = PredClause(AST, AST);                                        // PredClause ::= "predicate" FunIdentifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "axiom" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "theorem" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "proof" Identifier FunctionParamList
data AST = Dummy(AST);                                                  // FunctionParamList ::= "(" {FunctionParam ","}* ")"
data AST = Dummy(AST);                                                  // ProcedureParamList ::= "(" {ProcedureParam ","}* ")"
data AST = Param(AST, AST);                                             // FunctionParam ::= Type VarIdentifier
data AST = Param(AST, AST);                                             // ProcedureParam ::= Type VarIdentifier
data AST = ObsParam(AST, AST);                                          // ProcedureParam ::= Type Identifier
data AST = AnonParam(AST);                                              // ProcedureParam ::= Type
data AST = VarClause(AST, AST);                                         // VarClause ::= Type VarIdentifier
data AST = TypeClause(AST);                                             // TypeClause ::= "struct" TypeIdentifier
data AST = DotOp(AST, AST);                                             // Expr ::= Expr "." Identifier
data AST = DotOp(AST, AST);                                             // Expr ::= Expr "." DecNumeral
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr MULOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr ADDOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr SHFOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr RNGOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr CMPOP Expr
data AST = In(AST, AST);                                                // Expr ::= Expr "in" Expr
data AST = NotIn(AST, AST);                                             // Expr ::= Expr "not" "in" Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr EQUOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BTAND Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BTXOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BITOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr SUBST Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LGAND Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LOGOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LGIMP Expr
data AST = Index(AST, AST);                                             // Expr ::= Expr "[" {Expr ","}* "]"
data AST = PreOp(AST, AST);                                             // Expr ::= NEGOP Expr
data AST = PreOp(AST, AST);                                             // Expr ::= BTNOT Expr
data AST = PreOp(AST, AST);                                             // Expr ::= LGNOT Expr
data AST = CxxTree(AST);                                                // Program ::= Decl*

data AST = leaf(str strVal) | var(str name) | seq(list[AST] args);

anno loc AST@\loc;anno list[XaToken] AST@concrete;AST makeAST(str name, list[AST] args) {
switch(name) {
		case <"Name", [arg0]>: return Name(arg0);
		case <"Name", [arg0]>: return Name(arg0);
		case <"QName", [arg0, arg1]>: return QName(arg0, arg1);
		case <"Type", [arg0]>: return Type(arg0);
		case <"NilType", []>: return NilType();
		case <"Struct", [arg0]>: return Struct(arg0);
		case <"Int", [arg0]>: return Int(arg0);
		case <"Real", [arg0]>: return Real(arg0);
		case <"String", [arg0]>: return String(arg0);
		case <"Hex", [arg0]>: return Hex(arg0);
		case <"Bin", [arg0]>: return Bin(arg0);
		case <"Oct", [arg0]>: return Oct(arg0);
		case <"Block", [arg0]>: return Block(arg0);
		case <"Nop", []>: return Nop();
		case <"If", [arg0, arg1, arg2]>: return If(arg0, arg1, arg2);
		case <"While", [arg0, arg1]>: return While(arg0, arg1);
		case <"CxxFor", [arg0, arg1, arg2]>: return CxxFor(arg0, arg1, arg2);
		case <"Call", [arg0, arg1]>: return Call(arg0, arg1);
		case <"Proc", [arg0]>: return Proc(arg0);
		case <"Yield", [arg0]>: return Yield(arg0);
		case <"Break", []>: return Break();
		case <"Return", []>: return Return();
		case <"Assign", [arg0, arg1]>: return Assign(arg0, arg1);
		case <"Let", [arg0, arg1]>: return Let(arg0, arg1);
		case <"VarDef", [arg0, arg1, arg2]>: return VarDef(arg0, arg1, arg2);
		case <"Assert", [arg0, arg1]>: return Assert(arg0, arg1);
		case <"By", [arg0]>: return By(arg0);
		case <"By", [arg0]>: return By(arg0);
		case <"QED", []>: return QED();
		case <"Undefined", []>: return Undefined();
		case <"Var", [arg0]>: return Var(arg0);
		case <"Literal", [arg0]>: return Literal(arg0);
		case <"Apply", [arg0, arg1]>: return Apply(arg0, arg1);
		case <"Fun", [arg0]>: return Fun(arg0);
		case <"IfThenElseExpr", [arg0, arg1, arg2]>: return IfThenElseExpr(arg0, arg1, arg2);
		case <"ListCons", [arg0]>: return ListCons(arg0);
		case <"ListCons", [arg0, arg1]>: return ListCons(arg0, arg1);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"DeclBody", [arg0]>: return DeclBody(arg0);
		case <"ProcClause", [arg0, arg1]>: return ProcClause(arg0, arg1);
		case <"Assign", []>: return Assign();
		case <"FunClause", [arg0, arg1, arg2]>: return FunClause(arg0, arg1, arg2);
		case <"Guard", [arg0]>: return Guard(arg0);
		case <"PredClause", [arg0, arg1]>: return PredClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"Dummy", [arg0]>: return Dummy(arg0);
		case <"Dummy", [arg0]>: return Dummy(arg0);
		case <"Param", [arg0, arg1]>: return Param(arg0, arg1);
		case <"Param", [arg0, arg1]>: return Param(arg0, arg1);
		case <"ObsParam", [arg0, arg1]>: return ObsParam(arg0, arg1);
		case <"AnonParam", [arg0]>: return AnonParam(arg0);
		case <"VarClause", [arg0, arg1]>: return VarClause(arg0, arg1);
		case <"TypeClause", [arg0]>: return TypeClause(arg0);
		case <"DotOp", [arg0, arg1]>: return DotOp(arg0, arg1);
		case <"DotOp", [arg0, arg1]>: return DotOp(arg0, arg1);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"In", [arg0, arg1]>: return In(arg0, arg1);
		case <"NotIn", [arg0, arg1]>: return NotIn(arg0, arg1);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"BinOp", [arg0, arg1, arg2]>: return BinOp(arg0, arg1, arg2);
		case <"Index", [arg0, arg1]>: return Index(arg0, arg1);
		case <"PreOp", [arg0, arg1]>: return PreOp(arg0, arg1);
		case <"PreOp", [arg0, arg1]>: return PreOp(arg0, arg1);
		case <"PreOp", [arg0, arg1]>: return PreOp(arg0, arg1);
		case <"CxxTree", [arg0]>: return CxxTree(arg0);

}
}
