module MagnoliaAST

data AST = MagnoliaTree(AST, AST);                                      // Program ::= ModuleHead TopDecl*
data AST = ModuleHead(AST, AST);                                        // ModuleHead ::= "module" Name ModuleClause* ";"
data AST = Nop();                                                       // TopDecl ::= ";"
data AST = Name(AST);                                                   // Identifier ::= ID
data AST = Name(AST);                                                   // Name ::= ID
data AST = QName(AST, AST);                                             // Name ::= Name "." ID
data AST = Type(AST);                                                   // Type ::= Name
data AST = NilType();                                                   // Type ::= "(" ")"
data AST = AltType(AST, AST);                                           // Type ::= Type "|" Type
data AST = ProdType(AST, AST);                                          // ProdType ::= Type "," ProdType
data AST = Dummy(AST);                                                  // Type ::= "(" ProdType ")"
data AST = Struct(AST);                                                 // Type ::= "struct" DeclBody
data AST = ExternalType(AST, AST);                                      // Type ::= "\external" "(" Identifier "," Name ")"
data AST = Block(AST);                                                  // BlockStat ::= "{" Stat* "}"
data AST = Nop();                                                       // Stat ::= ";"
data AST = If(AST, AST, AST);                                           // Stat ::= "if" Expr "then" Stat* "else" Stat* "end"
data AST = While(AST, AST);                                             // Stat ::= "while" Expr "do" Stat* "end"
data AST = Open(AST, AST);                                              // Stat ::= "open" {Identifier ","}* "in" Stat* "end"
data AST = For(AST, AST, AST);                                          // Stat ::= "for" Identifier "in" Expr "do" Stat* "end"
data AST = Call(AST, AST);                                              // Stat ::= "call" Proc "(" {Expr ","}* ")" ";"
data AST = Proc(AST);                                                   // Proc ::= ProcName
data AST = Yield(AST);                                                  // Stat ::= "yield" Expr ";"
data AST = Break();                                                     // Stat ::= "break" ";"
data AST = Return();                                                    // Stat ::= "return" ";"
data AST = Return(AST);                                                 // Stat ::= "return" Expr ";"
data AST = Assign(AST, AST);                                            // Stat ::= Expr "=" Expr ";"
data AST = Let(AST, AST);                                               // Stat ::= "let" LetClause* "in" Stat* "end"
data AST = VarDef(AST, AST, AST);                                       // LetClause ::= "var" Identifier ":" Type "=" Expr ";"
data AST = Assert(AST, AST);                                            // Stat ::= "assert" Expr AssertClause* ";"
data AST = By(AST);                                                     // AssertClause ::= "by" Expr
data AST = By(AST);                                                     // AssertClause ::= "by" "simplify" Expr
data AST = QED();                                                       // AssertClause ::= "qed"
data AST = ExternalProc(AST, AST);                                      // Proc ::= "\external" "(" Identifier "," Name ")"
data AST = ProcOf(AST);                                                 // Proc ::= "\p" "(" Name ")"
data AST = Undefined();                                                 // Expr ::= "_"
data AST = Var(AST, AST);                                               // Var ::= Name ":" Type
data AST = Literal(AST);                                                // Expr ::= Literal
data AST = Tuple(AST);                                                  // Expr ::= "(" {Expr ","}* ")"
data AST = TypeExpr(AST);                                               // Expr ::= ":" Type
data AST = Struct(AST, AST);                                            // Expr ::= Type "$" "{" {InitSpec ","}* "}"
data AST = Struct(AST, AST);                                            // Expr ::= Type "{" {InitSpec ","}* "}"
data AST = Field(AST, AST);                                             // InitSpec ::= Identifier ":=" Expr
data AST = Apply(AST, AST);                                             // Expr ::= Fun "(" {Expr ","}* ")"
data AST = Fun(AST);                                                    // Fun ::= FunName
data AST = ExternalFun(AST, AST);                                       // Fun ::= "\external" "(" Identifier "," Name ")"
data AST = FunOf(AST);                                                  // Fun ::= "\f" "(" Name ")"
data AST = IfThenElseExpr(AST, AST, AST);                               // Expr ::= "if" Expr "then" Expr "else" Expr "end"
data AST = BlockExpr(AST);                                              // Expr ::= "{" Stat* "}"
data AST = NumRep(AST, AST);                                            // DataRep ::= DecNumeral ".." DecNumeral ";"
data AST = AliasType(AST);                                              // DataRep ::= "type" Type ";"
data AST = StructRep(AST);                                              // DataRep ::= "struct" "{" Decl* "}"
data AST = UnionRep(AST);                                               // DataRep ::= "union" "{" Decl* "}"
data AST = TermRep(AST);                                                // DataRep ::= {ConsSpec ","}* ";"
data AST = TermCons0(AST);                                              // ConsSpec ::= Identifier
data AST = TermCons(AST, AST);                                          // ConsSpec ::= Identifier "(" {DataRep ","}* ")"
data AST = StatDef(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* StatDeclarative SubClause* BlockStat
data AST = DefDeclNS(AST, AST, AST, AST);                               // Decl ::= Modifier* StatDeclarative SubClause* "=" Stat
data AST = DefDecl(AST, AST, AST, AST);                                 // Decl ::= Modifier* ExprDeclarative SubClause* "=" Expr ";"
data AST = DefDecl(AST, AST, AST, AST);                                 // Decl ::= Modifier* TypeDeclarative SubClause* "=" Type ";"
data AST = DefDeclNS(AST, AST, AST, AST);                               // Decl ::= Modifier* DeclDeclarative SubClause* "=" Decl
data AST = DeclDef(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
data AST = DeclBody(AST);                                               // DeclBody ::= "{" Decl* "}"
data AST = External();                                                  // SubClause ::= "external"
data AST = ProtectModifier();                                           // Modifier ::= "protect"
data AST = Attrs(AST);                                                  // AttrClause ::= "[" {Attribute ","}* "]"
data AST = Attr(AST, AST);                                              // Attribute ::= Name "(" {Expr ","}* ")"
data AST = ProcClause(AST, AST);                                        // ProcClause ::= "procedure" ProcIdentifier ProcedureParamList
data AST = Assign();                                                    // ProcIdentifier ::= "_=_"
data AST = FunClause(AST, AST, AST);                                    // FunClause ::= "function" FunIdentifier FunctionParamList ":" Type
data AST = Guard(AST);                                                  // GuardClause ::= "guard" Expr
data AST = PredClause(AST, AST);                                        // PredClause ::= "predicate" FunIdentifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "axiom" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "theorem" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "proof" Identifier FunctionParamList
data AST = ParamList(AST);                                              // FunctionParamList ::= "(" {FunctionParam ","}* ")"
data AST = ParamList(AST);                                              // ProcedureParamList ::= "(" {ProcedureParam ","}* ")"
data AST = Param(AST, AST);                                             // FunctionParam ::= VarIdentifier ":" Type
data AST = Param(AST, AST, AST);                                        // ProcedureParam ::= ParamMode VarIdentifier ":" Type
data AST = Obs();                                                       // ParamMode ::= "obs"
data AST = Upd();                                                       // ParamMode ::= "upd"
data AST = Out();                                                       // ParamMode ::= "out"
data AST = Exp();                                                       // ParamMode ::= "exp"
data AST = Giv();                                                       // ParamMode ::= "giv"
data AST = Del();                                                       // ParamMode ::= "del"
data AST = Nrm();                                                       // ParamMode ::= "nrm"
data AST = VarClause(AST, AST);                                         // VarClause ::= "var" VarIdentifier ":" Type
data AST = TypeClause(AST);                                             // TypeClause ::= "type" TypeIdentifier
data AST = DefaultModifier();                                           // Modifier ::= "default"
data AST = AbstractModifier();                                          // Modifier ::= "abstract"
data AST = ConceptDef(AST, AST, AST, AST);                              // TopDecl ::= Modifier* "concept" Identifier SubClause* "=" InstExpr
data AST = ImplDef(AST, AST, AST, AST);                                 // TopDecl ::= Modifier* "implementation" Identifier SubClause* "=" InstExpr
data AST = LibraryDef(AST, AST, AST, AST);                              // TopDecl ::= Modifier* "library" Identifier SubClause* "=" InstExpr
data AST = SatisfactionDef(AST, AST, AST, AST);                         // TopDecl ::= Modifier* "satisfaction" Identifier SubClause* "=" SatisfactionExpr
data AST = ImplDef(AST, AST, AST, AST);                                 // Decl ::= Modifier* "implementation" Identifier SubClause* ImplExpr
data AST = ConceptDef(AST, AST, AST, AST);                              // TopDecl ::= Modifier* "concept" Identifier SubClause* DeclBody
data AST = SignatureOf(AST);                                            // InstExpr ::= "signature" InstExpr
data AST = FullOf(AST);                                                 // InstExpr ::= "full" InstExpr
data AST = OnOf(AST);                                                   // InstExpr ::= "on" InstExpr
data AST = DeclaredOf(AST);                                             // InstExpr ::= "declared" InstExpr
data AST = Filtered(AST, AST);                                          // InstExpr ::= InstExpr FilterExpr
data AST = RetainFilter(AST);                                           // FilterExpr ::= "retain" AlgDeclOrName
data AST = RemoveFilter(AST);                                           // FilterExpr ::= "remove" AlgDeclOrName
data AST = Renamed(AST, AST);                                           // InstExpr ::= InstExpr "[" {Renaming ","}* "]"
data AST = RenameImpl(AST);                                             // InstExpr ::= "[" {Renaming ","}* "]"
data AST = Morphed(AST, AST);                                           // InstExpr ::= InstExpr "morphism" InstExpr
data AST = Protected(AST, AST);                                         // InstExpr ::= InstExpr "protect" AlgDeclOrName
data AST = OnDefines(AST, AST);                                         // InstExpr ::= "on" InstExpr "defines" AlgDecl
data AST = Defines(AST);                                                // InstExpr ::= "defines" AlgDecl
data AST = External(AST);                                               // InstExpr ::= "external" ExternalExpr
data AST = At(AST, AST);                                                // InstExpr ::= InstExpr "@" InstExpr
data AST = AtAt(AST, AST);                                              // InstExpr ::= InstExpr "@@" InstExpr
data AST = Plus(AST, AST);                                              // InstExpr ::= InstExpr "+" InstExpr
data AST = PlusPlus(AST, AST);                                          // InstExpr ::= InstExpr "++" InstExpr
data AST = TimesTimes(AST, AST);                                        // InstExpr ::= InstExpr "**" InstExpr
data AST = OnFilter(AST, AST);                                          // InstExpr ::= InstExpr "on" Filter
data AST = DeclaredFilter(AST, AST);                                    // InstExpr ::= InstExpr "declared" Filter
data AST = Times(AST, AST);                                             // InstExpr ::= InstExpr "times" InstExpr
data AST = DataInvariant(AST, AST);                                     // InstExpr ::= InstExpr "dataInvariant" InstExpr
data AST = Quotient(AST, AST);                                          // InstExpr ::= InstExpr "quotient" InstExpr
data AST = Homomorphism(AST, AST, AST, AST);                            // InstExpr ::= InstExpr "homomorphism" InstExpr "on" InstExpr "with" InstExpr
data AST = ExternalDefines(AST, AST, AST);                              // ExternalExpr ::= Identifier Name "defines" InstExpr
data AST = ExternalOnDefines(AST, AST, AST, AST);                       // ExternalExpr ::= Identifier Name "on" InstExpr "defines" InstExpr
data AST = ExternalExtendsOnDefines(AST, AST, AST, AST, AST);           // ExternalExpr ::= Identifier Name "extends" InstExpr "on" InstExpr "defines" InstExpr
data AST = Rename(AST, AST);                                            // Renaming ::= Name "=>" Name
data AST = Decls(AST);                                                  // AlgDecl ::= DeclBody
data AST = DefDeclNS(AST, AST, AST, AST);                               // SingleAlgDecl ::= Modifier* StatDeclarative SubClause* "=" BlockStat
data AST = DefDeclNS(AST, AST, AST, AST);                               // SingleAlgDecl ::= Modifier* ExprDeclarative SubClause* "=" Expr
data AST = DefDeclNS(AST, AST, AST, AST);                               // SingleAlgDecl ::= Modifier* TypeDeclarative SubClause* "=" Type
data AST = DefDeclNS(AST, AST, AST, AST);                               // SingleAlgDecl ::= Modifier* DeclDeclarative SubClause* "=" DeclBody
data AST = NoDefDeclNS(AST, AST, AST);                                  // SingleAlgDecl ::= Modifier* StatDeclarative SubClause*
data AST = NoDefDeclNS(AST, AST, AST);                                  // SingleAlgDecl ::= Modifier* ExprDeclarative SubClause*
data AST = NoDefDeclNS(AST, AST, AST);                                  // SingleAlgDecl ::= Modifier* TypeDeclarative SubClause*
data AST = NoDefDeclNS(AST, AST, AST);                                  // SingleAlgDecl ::= Modifier* DeclDeclarative SubClause*
data AST = Requires(AST);                                               // Decl ::= "requires" {InstExpr ","}+ ";"
data AST = Preserve(AST);                                               // Decl ::= "preserve" InstExpr ";"
data AST = Congruence(AST);                                             // Decl ::= "congruence" InstExpr ";"
data AST = PreserveOn(AST, AST);                                        // Decl ::= "preserve" InstExpr "on" InstExpr ";"
data AST = CongruenceOn(AST, AST);                                      // Decl ::= "congruence" InstExpr "on" InstExpr ";"
data AST = GenerateBy(AST, AST);                                        // Decl ::= "generate" {Type ","}+ "by" InstExpr ";"
data AST = GenerateBy(AST, AST);                                        // Decl ::= "generate" {Type ","}+ "by" {SingleAlgDecl ","}+ ";"
data AST = FreeBy(AST, AST);                                            // Decl ::= "free" {Type ","}+ "by" InstExpr ";"
data AST = FreeBy(AST, AST);                                            // Decl ::= "free" {Type ","}+ "by" {SingleAlgDecl ","}+ ";"
data AST = PartitionBy(AST, AST);                                       // Decl ::= "partition" {Type ","}+ "by" InstExpr ";"
data AST = PartitionBy(AST, AST);                                       // Decl ::= "partition" {Type ","}+ "by" {SingleAlgDecl ","}+ ";"
data AST = HomomorphismOnWith(AST, AST, AST);                           // Decl ::= "homomorphism" InstExpr "on" InstExpr "with" InstExpr ";"
data AST = Models(AST, AST);                                            // SatisfactionExpr ::= InstExpr "models" InstExpr
data AST = WithModels(AST, AST, AST);                                   // SatisfactionExpr ::= InstExpr "with" InstExpr "models" InstExpr
data AST = Hex(AST);                                                    // Literal ::= HexNumeral
data AST = Bin(AST);                                                    // Literal ::= BinNumeral
data AST = Oct(AST);                                                    // Literal ::= OctNumeral
data AST = BaseFor(AST, AST, AST);                                      // Stat ::= "for" Identifier "in" Expr Stat
data AST = BasePrintLn(AST);                                            // Stat ::= "print" {Expr ","}* ";"
data AST = BasePrint(AST);                                              // Stat ::= "print" {Expr ","}* "," ";"
data AST = BaseVarDefTI(AST, AST, AST);                                 // Stat ::= "var" Identifier ":" Type "=" Expr ";"
data AST = BaseVarDefI(AST, AST);                                       // Stat ::= "var" Identifier "=" Expr ";"
data AST = BaseVarDefI(AST);                                            // Stat ::= "var" Identifier ";"
data AST = BaseVarDefT(AST, AST);                                       // Stat ::= "var" Identifier ":" Type ";"
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
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* StatDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* ExprDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* TypeDeclarative SubClause* ";"
data AST = FunClause(AST, AST);                                         // FunClause ::= "define" FunName ":" Type
data AST = DataInvariant(AST);                                          // FunClause ::= "dataInvariant" FunctionParamList
data AST = DataInvariant(AST);                                          // ProcClause ::= "dataInvariant" ProcedureParamList
data AST = Congruence(AST);                                             // FunClause ::= "congruence" FunctionParamList
data AST = Congruence(AST);                                             // ProcClause ::= "congruence" ProcedureParamList
data AST = AnonParam(AST);                                              // FunctionParam ::= Type
data AST = ObsParam(AST, AST);                                          // ProcedureParam ::= Identifier ":" Type
data AST = AnonParam(AST, AST);                                         // ProcedureParam ::= ParamMode ":" Type
data AST = Attr(AST);                                                   // Attribute ::= Identifier
data AST = Opens(AST);                                                  // SubClause ::= "opens" "(" {Identifier ","}* ")"
data AST = Default(AST);                                                // SubClause ::= "default" "(" {Identifier ","}* ")"
data AST = SimpleModule();                                              // ModuleHead ::= 
data AST = Language(AST);                                               // ModuleClause ::= "language" {Name ","}*
data AST = Imports(AST);                                                // ModuleClause ::= "imports" {ImportClause ","}*
data AST = Requires(AST);                                               // ModuleClause ::= "requires" {RequiresClause ","}*
data AST = CompilePragma(AST, AST);                                     // ModuleClause ::= "compile" """ String """ "{" CompileClause* "}"
data AST = Pragma(AST, AST);                                            // CompileClause ::= ID "(" {QuotedString ","}* ")" ";"
data AST = Dummy(AST);                                                  // QuotedString ::= """ String """
data AST = ImportRequires(AST, AST);                                    // RequiresClause ::= Identifier "<" {ConceptClauseArgument ","}* ">"
data AST = ImportAll(AST);                                              // ImportClause ::= Name
data AST = Unresolved(AST);                                             // ConceptClauseArgument ::= Type
data AST = ImportModule(AST);                                           // ImportClause ::= "module" Name
data AST = ImportRename(AST, AST);                                      // ImportClause ::= "module" Name "=" Name
data AST = Nop();                                                       // Decl ::= ";"
data AST = UserSyntax0(AST, AST, AST);                                  // Stat ::= Expr "." Identifier "=" Expr ";"
data AST = UserSyntax1(AST, AST, AST);                                  // Stat ::= Identifier "[" {Expr ","}+ "]" "=" Expr ";"
data AST = UserSyntax2(AST);                                            // Expr ::= Name
data AST = UserSyntax3(AST, AST);                                       // Stat ::= "if" Expr "then" Stat* "end"
data AST = UserSyntax4(AST, AST);                                       // Stat ::= "if" "(" Expr ")" "{" Stat* "}"
data AST = UserSyntax5(AST, AST, AST);                                  // Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"

data AST = leaf(str strVal) | var(str name) | seq(list[AST] args);

data XaToken = token(str chars) | space(str chars) | comment(str chars) | child(int index) | sep(XaToken tok, str chars);
anno loc AST@\loc;
anno list[XaToken] AST@concrete;
AST makeAST(str name, list[AST] args) {
switch(name) {
		case <"MagnoliaTree", [arg0, arg1]>: return MagnoliaTree(arg0, arg1);
		case <"ModuleHead", [arg0, arg1]>: return ModuleHead(arg0, arg1);
		case <"Nop", []>: return Nop();
		case <"Name", [arg0]>: return Name(arg0);
		case <"Name", [arg0]>: return Name(arg0);
		case <"QName", [arg0, arg1]>: return QName(arg0, arg1);
		case <"Type", [arg0]>: return Type(arg0);
		case <"NilType", []>: return NilType();
		case <"AltType", [arg0, arg1]>: return AltType(arg0, arg1);
		case <"ProdType", [arg0, arg1]>: return ProdType(arg0, arg1);
		case <"Dummy", [arg0]>: return Dummy(arg0);
		case <"Struct", [arg0]>: return Struct(arg0);
		case <"ExternalType", [arg0, arg1]>: return ExternalType(arg0, arg1);
		case <"Block", [arg0]>: return Block(arg0);
		case <"Nop", []>: return Nop();
		case <"If", [arg0, arg1, arg2]>: return If(arg0, arg1, arg2);
		case <"While", [arg0, arg1]>: return While(arg0, arg1);
		case <"Open", [arg0, arg1]>: return Open(arg0, arg1);
		case <"For", [arg0, arg1, arg2]>: return For(arg0, arg1, arg2);
		case <"Call", [arg0, arg1]>: return Call(arg0, arg1);
		case <"Proc", [arg0]>: return Proc(arg0);
		case <"Yield", [arg0]>: return Yield(arg0);
		case <"Break", []>: return Break();
		case <"Return", []>: return Return();
		case <"Return", [arg0]>: return Return(arg0);
		case <"Assign", [arg0, arg1]>: return Assign(arg0, arg1);
		case <"Let", [arg0, arg1]>: return Let(arg0, arg1);
		case <"VarDef", [arg0, arg1, arg2]>: return VarDef(arg0, arg1, arg2);
		case <"Assert", [arg0, arg1]>: return Assert(arg0, arg1);
		case <"By", [arg0]>: return By(arg0);
		case <"By", [arg0]>: return By(arg0);
		case <"QED", []>: return QED();
		case <"ExternalProc", [arg0, arg1]>: return ExternalProc(arg0, arg1);
		case <"ProcOf", [arg0]>: return ProcOf(arg0);
		case <"Undefined", []>: return Undefined();
		case <"Var", [arg0, arg1]>: return Var(arg0, arg1);
		case <"Literal", [arg0]>: return Literal(arg0);
		case <"Tuple", [arg0]>: return Tuple(arg0);
		case <"TypeExpr", [arg0]>: return TypeExpr(arg0);
		case <"Struct", [arg0, arg1]>: return Struct(arg0, arg1);
		case <"Struct", [arg0, arg1]>: return Struct(arg0, arg1);
		case <"Field", [arg0, arg1]>: return Field(arg0, arg1);
		case <"Apply", [arg0, arg1]>: return Apply(arg0, arg1);
		case <"Fun", [arg0]>: return Fun(arg0);
		case <"ExternalFun", [arg0, arg1]>: return ExternalFun(arg0, arg1);
		case <"FunOf", [arg0]>: return FunOf(arg0);
		case <"IfThenElseExpr", [arg0, arg1, arg2]>: return IfThenElseExpr(arg0, arg1, arg2);
		case <"BlockExpr", [arg0]>: return BlockExpr(arg0);
		case <"NumRep", [arg0, arg1]>: return NumRep(arg0, arg1);
		case <"AliasType", [arg0]>: return AliasType(arg0);
		case <"StructRep", [arg0]>: return StructRep(arg0);
		case <"UnionRep", [arg0]>: return UnionRep(arg0);
		case <"TermRep", [arg0]>: return TermRep(arg0);
		case <"TermCons0", [arg0]>: return TermCons0(arg0);
		case <"TermCons", [arg0, arg1]>: return TermCons(arg0, arg1);
		case <"StatDef", [arg0, arg1, arg2, arg3]>: return StatDef(arg0, arg1, arg2, arg3);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"DefDecl", [arg0, arg1, arg2, arg3]>: return DefDecl(arg0, arg1, arg2, arg3);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"DeclDef", [arg0, arg1, arg2, arg3]>: return DeclDef(arg0, arg1, arg2, arg3);
		case <"DeclBody", [arg0]>: return DeclBody(arg0);
		case <"External", []>: return External();
		case <"ProtectModifier", []>: return ProtectModifier();
		case <"Attrs", [arg0]>: return Attrs(arg0);
		case <"Attr", [arg0, arg1]>: return Attr(arg0, arg1);
		case <"ProcClause", [arg0, arg1]>: return ProcClause(arg0, arg1);
		case <"Assign", []>: return Assign();
		case <"FunClause", [arg0, arg1, arg2]>: return FunClause(arg0, arg1, arg2);
		case <"Guard", [arg0]>: return Guard(arg0);
		case <"PredClause", [arg0, arg1]>: return PredClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"AxiomClause", [arg0, arg1]>: return AxiomClause(arg0, arg1);
		case <"ParamList", [arg0]>: return ParamList(arg0);
		case <"ParamList", [arg0]>: return ParamList(arg0);
		case <"Param", [arg0, arg1]>: return Param(arg0, arg1);
		case <"Param", [arg0, arg1, arg2]>: return Param(arg0, arg1, arg2);
		case <"Obs", []>: return Obs();
		case <"Upd", []>: return Upd();
		case <"Out", []>: return Out();
		case <"Exp", []>: return Exp();
		case <"Giv", []>: return Giv();
		case <"Del", []>: return Del();
		case <"Nrm", []>: return Nrm();
		case <"VarClause", [arg0, arg1]>: return VarClause(arg0, arg1);
		case <"TypeClause", [arg0]>: return TypeClause(arg0);
		case <"DefaultModifier", []>: return DefaultModifier();
		case <"AbstractModifier", []>: return AbstractModifier();
		case <"ConceptDef", [arg0, arg1, arg2, arg3]>: return ConceptDef(arg0, arg1, arg2, arg3);
		case <"ImplDef", [arg0, arg1, arg2, arg3]>: return ImplDef(arg0, arg1, arg2, arg3);
		case <"LibraryDef", [arg0, arg1, arg2, arg3]>: return LibraryDef(arg0, arg1, arg2, arg3);
		case <"SatisfactionDef", [arg0, arg1, arg2, arg3]>: return SatisfactionDef(arg0, arg1, arg2, arg3);
		case <"ImplDef", [arg0, arg1, arg2, arg3]>: return ImplDef(arg0, arg1, arg2, arg3);
		case <"ConceptDef", [arg0, arg1, arg2, arg3]>: return ConceptDef(arg0, arg1, arg2, arg3);
		case <"SignatureOf", [arg0]>: return SignatureOf(arg0);
		case <"FullOf", [arg0]>: return FullOf(arg0);
		case <"OnOf", [arg0]>: return OnOf(arg0);
		case <"DeclaredOf", [arg0]>: return DeclaredOf(arg0);
		case <"Filtered", [arg0, arg1]>: return Filtered(arg0, arg1);
		case <"RetainFilter", [arg0]>: return RetainFilter(arg0);
		case <"RemoveFilter", [arg0]>: return RemoveFilter(arg0);
		case <"Renamed", [arg0, arg1]>: return Renamed(arg0, arg1);
		case <"RenameImpl", [arg0]>: return RenameImpl(arg0);
		case <"Morphed", [arg0, arg1]>: return Morphed(arg0, arg1);
		case <"Protected", [arg0, arg1]>: return Protected(arg0, arg1);
		case <"OnDefines", [arg0, arg1]>: return OnDefines(arg0, arg1);
		case <"Defines", [arg0]>: return Defines(arg0);
		case <"External", [arg0]>: return External(arg0);
		case <"At", [arg0, arg1]>: return At(arg0, arg1);
		case <"AtAt", [arg0, arg1]>: return AtAt(arg0, arg1);
		case <"Plus", [arg0, arg1]>: return Plus(arg0, arg1);
		case <"PlusPlus", [arg0, arg1]>: return PlusPlus(arg0, arg1);
		case <"TimesTimes", [arg0, arg1]>: return TimesTimes(arg0, arg1);
		case <"OnFilter", [arg0, arg1]>: return OnFilter(arg0, arg1);
		case <"DeclaredFilter", [arg0, arg1]>: return DeclaredFilter(arg0, arg1);
		case <"Times", [arg0, arg1]>: return Times(arg0, arg1);
		case <"DataInvariant", [arg0, arg1]>: return DataInvariant(arg0, arg1);
		case <"Quotient", [arg0, arg1]>: return Quotient(arg0, arg1);
		case <"Homomorphism", [arg0, arg1, arg2, arg3]>: return Homomorphism(arg0, arg1, arg2, arg3);
		case <"ExternalDefines", [arg0, arg1, arg2]>: return ExternalDefines(arg0, arg1, arg2);
		case <"ExternalOnDefines", [arg0, arg1, arg2, arg3]>: return ExternalOnDefines(arg0, arg1, arg2, arg3);
		case <"ExternalExtendsOnDefines", [arg0, arg1, arg2, arg3, arg4]>: return ExternalExtendsOnDefines(arg0, arg1, arg2, arg3, arg4);
		case <"Rename", [arg0, arg1]>: return Rename(arg0, arg1);
		case <"Decls", [arg0]>: return Decls(arg0);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"DefDeclNS", [arg0, arg1, arg2, arg3]>: return DefDeclNS(arg0, arg1, arg2, arg3);
		case <"NoDefDeclNS", [arg0, arg1, arg2]>: return NoDefDeclNS(arg0, arg1, arg2);
		case <"NoDefDeclNS", [arg0, arg1, arg2]>: return NoDefDeclNS(arg0, arg1, arg2);
		case <"NoDefDeclNS", [arg0, arg1, arg2]>: return NoDefDeclNS(arg0, arg1, arg2);
		case <"NoDefDeclNS", [arg0, arg1, arg2]>: return NoDefDeclNS(arg0, arg1, arg2);
		case <"Requires", [arg0]>: return Requires(arg0);
		case <"Preserve", [arg0]>: return Preserve(arg0);
		case <"Congruence", [arg0]>: return Congruence(arg0);
		case <"PreserveOn", [arg0, arg1]>: return PreserveOn(arg0, arg1);
		case <"CongruenceOn", [arg0, arg1]>: return CongruenceOn(arg0, arg1);
		case <"GenerateBy", [arg0, arg1]>: return GenerateBy(arg0, arg1);
		case <"GenerateBy", [arg0, arg1]>: return GenerateBy(arg0, arg1);
		case <"FreeBy", [arg0, arg1]>: return FreeBy(arg0, arg1);
		case <"FreeBy", [arg0, arg1]>: return FreeBy(arg0, arg1);
		case <"PartitionBy", [arg0, arg1]>: return PartitionBy(arg0, arg1);
		case <"PartitionBy", [arg0, arg1]>: return PartitionBy(arg0, arg1);
		case <"HomomorphismOnWith", [arg0, arg1, arg2]>: return HomomorphismOnWith(arg0, arg1, arg2);
		case <"Models", [arg0, arg1]>: return Models(arg0, arg1);
		case <"WithModels", [arg0, arg1, arg2]>: return WithModels(arg0, arg1, arg2);
		case <"Hex", [arg0]>: return Hex(arg0);
		case <"Bin", [arg0]>: return Bin(arg0);
		case <"Oct", [arg0]>: return Oct(arg0);
		case <"BaseFor", [arg0, arg1, arg2]>: return BaseFor(arg0, arg1, arg2);
		case <"BasePrintLn", [arg0]>: return BasePrintLn(arg0);
		case <"BasePrint", [arg0]>: return BasePrint(arg0);
		case <"BaseVarDefTI", [arg0, arg1, arg2]>: return BaseVarDefTI(arg0, arg1, arg2);
		case <"BaseVarDefI", [arg0, arg1]>: return BaseVarDefI(arg0, arg1);
		case <"BaseVarDefI", [arg0]>: return BaseVarDefI(arg0);
		case <"BaseVarDefT", [arg0, arg1]>: return BaseVarDefT(arg0, arg1);
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
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"NoDefDecl", [arg0, arg1, arg2]>: return NoDefDecl(arg0, arg1, arg2);
		case <"FunClause", [arg0, arg1]>: return FunClause(arg0, arg1);
		case <"DataInvariant", [arg0]>: return DataInvariant(arg0);
		case <"DataInvariant", [arg0]>: return DataInvariant(arg0);
		case <"Congruence", [arg0]>: return Congruence(arg0);
		case <"Congruence", [arg0]>: return Congruence(arg0);
		case <"AnonParam", [arg0]>: return AnonParam(arg0);
		case <"ObsParam", [arg0, arg1]>: return ObsParam(arg0, arg1);
		case <"AnonParam", [arg0, arg1]>: return AnonParam(arg0, arg1);
		case <"Attr", [arg0]>: return Attr(arg0);
		case <"Opens", [arg0]>: return Opens(arg0);
		case <"Default", [arg0]>: return Default(arg0);
		case <"SimpleModule", []>: return SimpleModule();
		case <"Language", [arg0]>: return Language(arg0);
		case <"Imports", [arg0]>: return Imports(arg0);
		case <"Requires", [arg0]>: return Requires(arg0);
		case <"CompilePragma", [arg0, arg1]>: return CompilePragma(arg0, arg1);
		case <"Pragma", [arg0, arg1]>: return Pragma(arg0, arg1);
		case <"Dummy", [arg0]>: return Dummy(arg0);
		case <"ImportRequires", [arg0, arg1]>: return ImportRequires(arg0, arg1);
		case <"ImportAll", [arg0]>: return ImportAll(arg0);
		case <"Unresolved", [arg0]>: return Unresolved(arg0);
		case <"ImportModule", [arg0]>: return ImportModule(arg0);
		case <"ImportRename", [arg0, arg1]>: return ImportRename(arg0, arg1);
		case <"Nop", []>: return Nop();
		case <"UserSyntax0", [arg0, arg1, arg2]>: return UserSyntax0(arg0, arg1, arg2);
		case <"UserSyntax1", [arg0, arg1, arg2]>: return UserSyntax1(arg0, arg1, arg2);
		case <"UserSyntax2", [arg0]>: return UserSyntax2(arg0);
		case <"UserSyntax3", [arg0, arg1]>: return UserSyntax3(arg0, arg1);
		case <"UserSyntax4", [arg0, arg1]>: return UserSyntax4(arg0, arg1);
		case <"UserSyntax5", [arg0, arg1, arg2]>: return UserSyntax5(arg0, arg1, arg2);

}
}
