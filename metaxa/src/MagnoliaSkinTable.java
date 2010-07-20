package org.magnolialang.xatree;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.imp.pdb.facts.IList;
import static org.magnolialang.xatree.XaTreeFactory.*;

class MagnoliaSkinTable {
	public static Map<String,IList> getMap() {
		Map<String,IList> tbl = new HashMap<String,IList>();

		// InstExpr ::= InstExpr "@" InstExpr
		tbl.put("At/2:InstExpr", vf.list(child(0), space(" "), token("@"), space(" "), child(1)));

		// Stat ::= "yield" Expr ";"
		tbl.put("Yield/1:Stat", vf.list(token("yield"), space(" "), child(0), space(" "), token(";")));

		// Decl ::= Modifier* "implementation" Identifier SubClause* ImplExpr
		tbl.put("ImplDef/4:Decl", vf.list(child(0), space(" "), token("implementation"), space(" "), child(1), space(" "), child(2), space(" "), child(3)));

		// SingleAlgDecl ::= Modifier* StatDeclarative SubClause* "=" BlockStat
		tbl.put("StatDef/4:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Literal ::= OctNumeral
		tbl.put("Oct/1:Literal", vf.list(child(0)));

		// PredClause ::= "predicate" FunIdentifier FunctionParamList
		tbl.put("PredClause/2:PredClause", vf.list(token("predicate"), space(" "), child(0), space(" "), child(1)));

		// Stat ::= "var" Identifier "=" Expr ";"
		tbl.put("BaseVarDefI/2:Stat", vf.list(token("var"), space(" "), child(0), space(" "), token("="), space(" "), child(1), space(" "), token(";")));

		// Decl ::= "congruence" InstExpr ";"
		tbl.put("Congruence/1:Decl", vf.list(token("congruence"), space(" "), child(0), space(" "), token(";")));

		// SingleAlgDecl ::= Modifier* TypeDeclarative SubClause*
		tbl.put("TypeDecl/3:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// ProcClause ::= "congruence" ProcedureParamList
		tbl.put("Congruence/1:ProcClause", vf.list(token("congruence"), space(" "), child(0)));

		// Expr ::= Name
		tbl.put("UserSyntax2/1:Expr", vf.list(child(0)));

		// Fun ::= FunName
		tbl.put("Fun/1:Fun", vf.list(child(0)));

		// Name ::= Name "." ID
		tbl.put("QName/2:Name", vf.list(child(0), space(" "), token("."), space(" "), child(1)));

		// DataRep ::= "type" Type ";"
		tbl.put("AliasType/1:DataRep", vf.list(token("type"), space(" "), child(0), space(" "), token(";")));

		// SingleAlgDecl ::= Modifier* DeclDeclarative SubClause* "=" DeclBody
		tbl.put("DeclDef/4:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// InstExpr ::= InstExpr "protect" AlgDecl
		tbl.put("Protected/2:InstExpr", vf.list(child(0), space(" "), token("protect"), space(" "), child(1)));

		// SubClause ::= "opens" "(" {Identifier ","}* ")"
		tbl.put("Opens/1:SubClause", vf.list(token("opens"), space(" "), token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// ParamMode ::= "nrm"
		tbl.put("Nrm/0:ParamMode", vf.list(token("nrm")));

		// ParamMode ::= "upd"
		tbl.put("Upd/0:ParamMode", vf.list(token("upd")));

		// FunctionParam ::= Type
		tbl.put("AnonParam/1:FunctionParam", vf.list(child(0)));

		// Expr ::= "_"
		tbl.put("Undefined/0:Expr", vf.list(token("_")));

		// Stat ::= ";"
		tbl.put("Nop/0:Stat", vf.list(token(";")));

		// Program ::= ModuleHead TopDecl*
		tbl.put("MagnoliaTree/2:Program", vf.list(child(0), space(" "), child(1)));

		// Proc ::= ProcName
		tbl.put("Proc/1:Proc", vf.list(child(0)));

		// StringLiteral ::= """ String """
		tbl.put("String/1:StringLiteral", vf.list(token("\""), space(" "), child(0), space(" "), token("\"")));

		// VarClause ::= "var" VarIdentifier ":" Type
		tbl.put("VarClause/2:VarClause", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1)));

		// TopDecl ::= ";"
		tbl.put("Nop/0:TopDecl", vf.list(token(";")));

		// Decl ::= "requires" {InstExpr ","}+ ";"
		tbl.put("Requires/1:Decl", vf.list(token("requires"), space(" "), sep(child(0),","), space(" "), token(";")));

		// TopDecl ::= Modifier* "library" Identifier SubClause* "=" InstExpr
		tbl.put("LibraryDef/4:TopDecl", vf.list(child(0), space(" "), token("library"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// ParamMode ::= "exp"
		tbl.put("Exp/0:ParamMode", vf.list(token("exp")));

		// Type ::= Type "|" Type
		tbl.put("AltType/2:Type", vf.list(child(0), space(" "), token("|"), space(" "), child(1)));

		// ConceptClauseArgument ::= Type
		tbl.put("Unresolved/1:ConceptClauseArgument", vf.list(child(0)));

		// QuotedString ::= """ String """
		tbl.put("Dummy/1:QuotedString", vf.list(token("\""), space(" "), child(0), space(" "), token("\"")));

		// DataRep ::= "union" "{" Decl* "}"
		tbl.put("UnionRep/1:DataRep", vf.list(token("union"), space(" "), token("{"), space(" "), child(0), space(" "), token("}")));

		// Stat ::= Expr "." Identifier "=" Expr ";"
		tbl.put("UserSyntax0/3:Stat", vf.list(child(0), space(" "), token("."), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// ProcClause ::= "dataInvariant" ProcedureParamList
		tbl.put("DataInvariant/1:ProcClause", vf.list(token("dataInvariant"), space(" "), child(0)));

		// InstExpr ::= InstExpr "quotient" InstExpr
		tbl.put("Quotient/2:InstExpr", vf.list(child(0), space(" "), token("quotient"), space(" "), child(1)));

		// Stat ::= "if" Expr "then" Stat* "end"
		tbl.put("UserSyntax3/2:Stat", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("end")));

		// Expr ::= Expr LGIMP Expr
		tbl.put("BinOp/3:Expr", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// ParamMode ::= "del"
		tbl.put("Del/0:ParamMode", vf.list(token("del")));

		// Expr ::= "[" {Expr ","}* "]"
		tbl.put("ListCons/1:Expr", vf.list(token("["), space(" "), sep(child(0),","), space(" "), token("]")));

		// SemiDecl ::= Modifier* ExprDeclarative SubClause* "=" Expr ";"
		tbl.put("DefDecl/4:SemiDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3), space(" "), token(";")));

		// AssertClause ::= "qed"
		tbl.put("QED/0:AssertClause", vf.list(token("qed")));

		// Stat ::= Identifier "=" Expr ";"
		tbl.put("Assign/2:Stat", vf.list(child(0), space(" "), token("="), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "return" Expr ";"
		tbl.put("Return/1:Stat", vf.list(token("return"), space(" "), child(0), space(" "), token(";")));

		// SingleAlgDecl ::= Modifier* ExprDeclarative SubClause* "=" Expr
		tbl.put("ExprDef/4:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// SubClause ::= "default" "(" {Identifier ","}* ")"
		tbl.put("Default/1:SubClause", vf.list(token("default"), space(" "), token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// Var ::= Name ":" Type
		tbl.put("Var/2:Var", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// Expr ::= Type "$" "{" {InitSpec ","}* "}"
		tbl.put("Struct/2:Expr", vf.list(child(0), space(" "), token("$"), space(" "), token("{"), space(" "), sep(child(1),","), space(" "), token("}")));

		// BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
		tbl.put("DefDecl/4:BraceDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), child(3)));

		// BlockStat ::= "{" Stat* "}"
		tbl.put("Block/1:BlockStat", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// AssertClause ::= "by" Expr
		tbl.put("By/1:AssertClause", vf.list(token("by"), space(" "), child(0)));

		// Decl ::= "generate" {Type ","}+ "by" InstExpr ";"
		tbl.put("GenerateBy/2:Decl", vf.list(token("generate"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// Decl ::= Modifier* TypeDeclarative SubClause* ";"
		tbl.put("NoDefDecl/3:Decl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token(";")));

		// LetClause ::= "var" Identifier ":" Type "=" Expr ";"
		tbl.put("VarDef/3:LetClause", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// SatisfactionExpr ::= InstExpr "models" InstExpr
		tbl.put("Models/2:SatisfactionExpr", vf.list(child(0), space(" "), token("models"), space(" "), child(1)));

		// Stat ::= "break" ";"
		tbl.put("Break/0:Stat", vf.list(token("break"), space(" "), token(";")));

		// InstExpr ::= InstExpr "[" {Renaming ","}* "]"
		tbl.put("Renamed/2:InstExpr", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]")));

		// FunClause ::= "congruence" FunctionParamList
		tbl.put("Congruence/1:FunClause", vf.list(token("congruence"), space(" "), child(0)));

		// InitSpec ::= Identifier ":=" Expr
		tbl.put("Field/2:InitSpec", vf.list(child(0), space(" "), token(":="), space(" "), child(1)));

		// Stat ::= Identifier "[" {Expr ","}+ "]" "=" Expr ";"
		tbl.put("UserSyntax1/3:Stat", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]"), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// ModuleHead ::= 
		tbl.put("SimpleModule/0:ModuleHead", vf.list());

		// SatisfactionExpr ::= InstExpr "with" InstExpr "models" InstExpr
		tbl.put("WithModels/3:SatisfactionExpr", vf.list(child(0), space(" "), token("with"), space(" "), child(1), space(" "), token("models"), space(" "), child(2)));

		// ExternalExpr ::= Identifier Name "on" InstExpr "defines" InstExpr
		tbl.put("ExternalOnDefines/4:ExternalExpr", vf.list(child(0), space(" "), child(1), space(" "), token("on"), space(" "), child(2), space(" "), token("defines"), space(" "), child(3)));

		// Stat ::= "if" "(" Expr ")" "{" Stat* "}"
		tbl.put("UserSyntax4/2:Stat", vf.list(token("if"), space(" "), token("("), space(" "), child(0), space(" "), token(")"), space(" "), token("{"), space(" "), child(1), space(" "), token("}")));

		// ModuleClause ::= "imports" {ImportClause ","}*
		tbl.put("Imports/1:ModuleClause", vf.list(token("imports"), space(" "), sep(child(0),",")));

		// FunctionParamList ::= "(" {FunctionParam ","}* ")"
		tbl.put("Dummy/1:FunctionParamList", vf.list(token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// InstExpr ::= "declared" InstExpr
		tbl.put("DeclaredOf/1:InstExpr", vf.list(token("declared"), space(" "), child(0)));

		// Decl ::= "free" {Type ","}+ "by" InstExpr ";"
		tbl.put("FreeBy/2:Decl", vf.list(token("free"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// InstExpr ::= InstExpr FilterExpr
		tbl.put("Filtered/2:InstExpr", vf.list(child(0), space(" "), child(1)));

		// Decl ::= "homomorphism" InstExpr "on" InstExpr "with" InstExpr ";"
		tbl.put("HomomorphismOnWith/3:Decl", vf.list(token("homomorphism"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token("with"), space(" "), child(2), space(" "), token(";")));

		// Modifier ::= "abstract"
		tbl.put("AbstractModifier/0:Modifier", vf.list(token("abstract")));

		// Identifier ::= ID
		tbl.put("Name/1:Identifier", vf.list(child(0)));

		// InstExpr ::= InstExpr "@@" InstExpr
		tbl.put("AtAt/2:InstExpr", vf.list(child(0), space(" "), token("@@"), space(" "), child(1)));

		// FunctionParam ::= VarIdentifier ":" Type
		tbl.put("Param/2:FunctionParam", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// ExternalExpr ::= Identifier Name "extends" InstExpr "on" InstExpr "defines" InstExpr
		tbl.put("ExternalExtendsOnDefines/5:ExternalExpr", vf.list(child(0), space(" "), child(1), space(" "), token("extends"), space(" "), child(2), space(" "), token("on"), space(" "), child(3), space(" "), token("defines"), space(" "), child(4)));

		// FilterExpr ::= "remove" InstExpr
		tbl.put("RemoveFilter/1:FilterExpr", vf.list(token("remove"), space(" "), child(0)));

		// Stat ::= "var" Identifier ":" Type ";"
		tbl.put("BaseVarDefT/2:Stat", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token(";")));

		// FunClause ::= "define" FunName ":" Type
		tbl.put("FunClause/2:FunClause", vf.list(token("define"), space(" "), child(0), space(" "), token(":"), space(" "), child(1)));

		// ParamMode ::= "out"
		tbl.put("Out/0:ParamMode", vf.list(token("out")));

		// Stat ::= "assert" Expr AssertClause* ";"
		tbl.put("Assert/2:Stat", vf.list(token("assert"), space(" "), child(0), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "for" Identifier "in" Expr "do" Stat* "end"
		tbl.put("For/3:Stat", vf.list(token("for"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), token("do"), space(" "), child(2), space(" "), token("end")));

		// GuardClause ::= "guard" Expr
		tbl.put("Guard/1:GuardClause", vf.list(token("guard"), space(" "), child(0)));

		// InstExpr ::= InstExpr "declared" Filter
		tbl.put("DeclaredFilter/2:InstExpr", vf.list(child(0), space(" "), token("declared"), space(" "), child(1)));

		// ImportClause ::= "module" Name
		tbl.put("ImportModule/1:ImportClause", vf.list(token("module"), space(" "), child(0)));

		// InstExpr ::= InstExpr "on" Filter
		tbl.put("OnFilter/2:InstExpr", vf.list(child(0), space(" "), token("on"), space(" "), child(1)));

		// Stat ::= "while" Expr "do" Stat* "end"
		tbl.put("While/2:Stat", vf.list(token("while"), space(" "), child(0), space(" "), token("do"), space(" "), child(1), space(" "), token("end")));

		// InstExpr ::= "on" InstExpr "defines" AlgDecl
		tbl.put("OnDefines/2:InstExpr", vf.list(token("on"), space(" "), child(0), space(" "), token("defines"), space(" "), child(1)));

		// ModuleHead ::= "module" Name ModuleClause* ";"
		tbl.put("ModuleHead/2:ModuleHead", vf.list(token("module"), space(" "), child(0), space(" "), child(1), space(" "), token(";")));

		// Decl ::= "preserve" InstExpr ";"
		tbl.put("Preserve/1:Decl", vf.list(token("preserve"), space(" "), child(0), space(" "), token(";")));

		// Attribute ::= Identifier
		tbl.put("Attr/1:Attribute", vf.list(child(0)));

		// DataRep ::= DecNumeral ".." DecNumeral ";"
		tbl.put("NumRep/2:DataRep", vf.list(child(0), space(" "), token(".."), space(" "), child(1), space(" "), token(";")));

		// InstExpr ::= InstExpr "**" InstExpr
		tbl.put("TimesTimes/2:InstExpr", vf.list(child(0), space(" "), token("**"), space(" "), child(1)));

		// ConsSpec ::= Identifier
		tbl.put("TermCons0/1:ConsSpec", vf.list(child(0)));

		// Decl ::= "congruence" InstExpr "on" InstExpr ";"
		tbl.put("CongruenceOn/2:Decl", vf.list(token("congruence"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "print" {Expr ","}* ";"
		tbl.put("BasePrintLn/1:Stat", vf.list(token("print"), space(" "), sep(child(0),","), space(" "), token(";")));

		// InstExpr ::= "full" InstExpr
		tbl.put("FullOf/1:InstExpr", vf.list(token("full"), space(" "), child(0)));

		// Stat ::= "call" Proc "(" {Expr ","}* ")" ";"
		tbl.put("Call/2:Stat", vf.list(token("call"), space(" "), child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")"), space(" "), token(";")));

		// Stat ::= "if" Expr "then" Stat* "else" Stat* "end"
		tbl.put("If/3:Stat", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("else"), space(" "), child(2), space(" "), token("end")));

		// InstExpr ::= InstExpr "homomorphism" InstExpr "on" InstExpr "with" InstExpr
		tbl.put("Homomorphism/4:InstExpr", vf.list(child(0), space(" "), token("homomorphism"), space(" "), child(1), space(" "), token("on"), space(" "), child(2), space(" "), token("with"), space(" "), child(3)));

		// Expr ::= "{" Stat* "}"
		tbl.put("BlockExpr/1:Expr", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// Type ::= "(" ProdType ")"
		tbl.put("Dummy/1:Type", vf.list(token("("), space(" "), child(0), space(" "), token(")")));

		// FilterExpr ::= "retain" InstExpr
		tbl.put("RetainFilter/1:FilterExpr", vf.list(token("retain"), space(" "), child(0)));

		// ProcedureParamList ::= "(" {ProcedureParam ","}* ")"
		tbl.put("Dummy/1:ProcedureParamList", vf.list(token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// CompileClause ::= ID "(" {QuotedString ","}* ")" ";"
		tbl.put("Pragma/2:CompileClause", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")"), space(" "), token(";")));

		// Expr ::= Expr "in" Expr
		tbl.put("In/2:Expr", vf.list(child(0), space(" "), token("in"), space(" "), child(1)));

		// AxiomClause ::= "axiom" Identifier FunctionParamList
		tbl.put("AxiomClause/2:AxiomClause", vf.list(token("axiom"), space(" "), child(0), space(" "), child(1)));

		// Stat ::= "var" Identifier ":" Type "=" Expr ";"
		tbl.put("BaseVarDefTI/3:Stat", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// ConsSpec ::= Identifier "(" {DataRep ","}* ")"
		tbl.put("TermCons/2:ConsSpec", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// TopDecl ::= Modifier* "satisfaction" Identifier SubClause* "=" SatisfactionExpr
		tbl.put("SatisfactionDef/4:TopDecl", vf.list(child(0), space(" "), token("satisfaction"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Expr ::= ":" Type
		tbl.put("TypeExpr/1:Expr", vf.list(token(":"), space(" "), child(0)));

		// Expr ::= "[" {Expr ","}* "|" Expr "]"
		tbl.put("ListCons/2:Expr", vf.list(token("["), space(" "), sep(child(0),","), space(" "), token("|"), space(" "), child(1), space(" "), token("]")));

		// Type ::= "struct" DeclBody
		tbl.put("Struct/1:Type", vf.list(token("struct"), space(" "), child(0)));

		// SingleAlgDecl ::= Modifier* DeclDeclarative SubClause*
		tbl.put("DeclDecl/3:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// InstExpr ::= InstExpr "dataInvariant" InstExpr
		tbl.put("DataInvariant/2:InstExpr", vf.list(child(0), space(" "), token("dataInvariant"), space(" "), child(1)));

		// Expr ::= Expr "[" {Expr ","}* "]"
		tbl.put("Index/2:Expr", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]")));

		// Type ::= Name
		tbl.put("Type/1:Type", vf.list(child(0)));

		// Modifier ::= "default"
		tbl.put("DefaultModifier/0:Modifier", vf.list(token("default")));

		// AlgDecl ::= DeclBody
		tbl.put("Decls/1:AlgDecl", vf.list(child(0)));

		// SingleAlgDecl ::= Modifier* TypeDeclarative SubClause* "=" Type
		tbl.put("TypeDef/4:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Stat ::= "var" Identifier ";"
		tbl.put("BaseVarDefI/1:Stat", vf.list(token("var"), space(" "), child(0), space(" "), token(";")));

		// Stat ::= "print" {Expr ","}* "," ";"
		tbl.put("BasePrint/1:Stat", vf.list(token("print"), space(" "), sep(child(0),","), space(" "), token(","), space(" "), token(";")));

		// Stat ::= "let" LetClause* "in" Stat* "end"
		tbl.put("Let/2:Stat", vf.list(token("let"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), token("end")));

		// TopDecl ::= Modifier* "implementation" Identifier SubClause* "=" InstExpr
		tbl.put("ImplDef/4:TopDecl", vf.list(child(0), space(" "), token("implementation"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// InstExpr ::= InstExpr "times" InstExpr
		tbl.put("Times/2:InstExpr", vf.list(child(0), space(" "), token("times"), space(" "), child(1)));

		// InstExpr ::= InstExpr "morphism" InstExpr
		tbl.put("Morphed/2:InstExpr", vf.list(child(0), space(" "), token("morphism"), space(" "), child(1)));

		// ProcIdentifier ::= "_=_"
		tbl.put("Assign/0:ProcIdentifier", vf.list(token("_=_")));

		// InstExpr ::= "external" ExternalExpr
		tbl.put("External/1:InstExpr", vf.list(token("external"), space(" "), child(0)));

		// TopDecl ::= Modifier* "concept" Identifier SubClause* "=" InstExpr
		tbl.put("ConceptDef/4:TopDecl", vf.list(child(0), space(" "), token("concept"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// FunClause ::= "function" FunIdentifier FunctionParamList ":" Type
		tbl.put("FunClause/3:FunClause", vf.list(token("function"), space(" "), child(0), space(" "), child(1), space(" "), token(":"), space(" "), child(2)));

		// ModuleClause ::= "compile" """ String """ "{" CompileClause* "}"
		tbl.put("CompilePragma/2:ModuleClause", vf.list(token("compile"), space(" "), token("\""), space(" "), child(0), space(" "), token("\""), space(" "), token("{"), space(" "), child(1), space(" "), token("}")));

		// DataRep ::= "struct" "{" Decl* "}"
		tbl.put("StructRep/1:DataRep", vf.list(token("struct"), space(" "), token("{"), space(" "), child(0), space(" "), token("}")));

		// Expr ::= Literal
		tbl.put("Literal/1:Expr", vf.list(child(0)));

		// Expr ::= Fun "(" {Expr ","}* ")"
		tbl.put("Apply/2:Expr", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// InstExpr ::= InstExpr "++" InstExpr
		tbl.put("PlusPlus/2:InstExpr", vf.list(child(0), space(" "), token("++"), space(" "), child(1)));

		// ModuleClause ::= "language" {Name ","}*
		tbl.put("Language/1:ModuleClause", vf.list(token("language"), space(" "), sep(child(0),",")));

		// InstExpr ::= InstExpr "+" InstExpr
		tbl.put("Plus/2:InstExpr", vf.list(child(0), space(" "), token("+"), space(" "), child(1)));

		// TypeClause ::= "type" TypeIdentifier
		tbl.put("TypeClause/1:TypeClause", vf.list(token("type"), space(" "), child(0)));

		// Name ::= ID
		tbl.put("Name/1:Name", vf.list(child(0)));

		// ParamMode ::= "obs"
		tbl.put("Obs/0:ParamMode", vf.list(token("obs")));

		// AttrClause ::= "[" {Attribute ","}* "]"
		tbl.put("Attrs/1:AttrClause", vf.list(token("["), space(" "), sep(child(0),","), space(" "), token("]")));

		// Attribute ::= Name "(" {Expr ","}* ")"
		tbl.put("Attr/2:Attribute", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// Decl ::= "preserve" InstExpr "on" InstExpr ";"
		tbl.put("PreserveOn/2:Decl", vf.list(token("preserve"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token(";")));

		// Literal ::= FloatNumeral
		tbl.put("Real/1:Literal", vf.list(child(0)));

		// Decl ::= ";"
		tbl.put("Nop/0:Decl", vf.list(token(";")));

		// Renaming ::= Name "=>" Name
		tbl.put("Rename/2:Renaming", vf.list(child(0), space(" "), token("=>"), space(" "), child(1)));

		// Stat ::= "for" Identifier "in" Expr Stat
		tbl.put("BaseFor/3:Stat", vf.list(token("for"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), child(2)));

		// Literal ::= HexNumeral
		tbl.put("Hex/1:Literal", vf.list(child(0)));

		// ProcedureParam ::= ParamMode ":" Type
		tbl.put("AnonParam/2:ProcedureParam", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// Decl ::= Modifier* TypeDeclarative SubClause* "=" Type ";"
		tbl.put("DefDecl/4:Decl", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3), space(" "), token(";")));

		// FunClause ::= "dataInvariant" FunctionParamList
		tbl.put("DataInvariant/1:FunClause", vf.list(token("dataInvariant"), space(" "), child(0)));

		// Type ::= "(" ")"
		tbl.put("NilType/0:Type", vf.list(token("("), space(" "), token(")")));

		// SingleAlgDecl ::= Modifier* ExprDeclarative SubClause*
		tbl.put("ExprDecl/3:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// ModuleClause ::= "requires" {RequiresClause ","}*
		tbl.put("Requires/1:ModuleClause", vf.list(token("requires"), space(" "), sep(child(0),",")));

		// ProcedureParam ::= Identifier ":" Type
		tbl.put("ObsParam/2:ProcedureParam", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// InstExpr ::= "signature" InstExpr
		tbl.put("SignatureOf/1:InstExpr", vf.list(token("signature"), space(" "), child(0)));

		// Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"
		tbl.put("UserSyntax5/3:Stat", vf.list(token("if"), space(" "), token("("), space(" "), child(0), space(" "), token(")"), space(" "), token("{"), space(" "), child(1), space(" "), token("}"), space(" "), token("else"), space(" "), token("{"), space(" "), child(2), space(" "), token("}")));

		// ProcedureParam ::= ParamMode VarIdentifier ":" Type
		tbl.put("Param/3:ProcedureParam", vf.list(child(0), space(" "), child(1), space(" "), token(":"), space(" "), child(2)));

		// Expr ::= Expr "." DecNumeral
		tbl.put("DotOp/2:Expr", vf.list(child(0), space(" "), token("."), space(" "), child(1)));

		// RequiresClause ::= Identifier "<" {ConceptClauseArgument ","}* ">"
		tbl.put("ImportRequires/2:RequiresClause", vf.list(child(0), space(" "), token("<"), space(" "), sep(child(1),","), space(" "), token(">")));

		// Decl ::= "partition" {Type ","}+ "by" InstExpr ";"
		tbl.put("PartitionBy/2:Decl", vf.list(token("partition"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// Literal ::= BinNumeral
		tbl.put("Bin/1:Literal", vf.list(child(0)));

		// InstExpr ::= "on" InstExpr
		tbl.put("OnOf/1:InstExpr", vf.list(token("on"), space(" "), child(0)));

		// Stat ::= "return" ";"
		tbl.put("Return/0:Stat", vf.list(token("return"), space(" "), token(";")));

		// InstExpr ::= "defines" AlgDecl
		tbl.put("Defines/1:InstExpr", vf.list(token("defines"), space(" "), child(0)));

		// DeclBody ::= "{" Decl* "}"
		tbl.put("DeclBody/1:DeclBody", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// ProdType ::= Type "," ProdType
		tbl.put("ProdType/2:ProdType", vf.list(child(0), space(" "), token(","), space(" "), child(1)));

		// ProcClause ::= "procedure" ProcIdentifier ProcedureParamList
		tbl.put("ProcClause/2:ProcClause", vf.list(token("procedure"), space(" "), child(0), space(" "), child(1)));

		// Expr ::= Expr "not" "in" Expr
		tbl.put("NotIn/2:Expr", vf.list(child(0), space(" "), token("not"), space(" "), token("in"), space(" "), child(1)));

		// ImportClause ::= Name
		tbl.put("ImportAll/1:ImportClause", vf.list(child(0)));

		// DataRep ::= {ConsSpec ","}* ";"
		tbl.put("TermRep/1:DataRep", vf.list(sep(child(0),","), space(" "), token(";")));

		// ParamMode ::= "giv"
		tbl.put("Giv/0:ParamMode", vf.list(token("giv")));

		// Modifier ::= "protect"
		tbl.put("ProtectModifier/0:Modifier", vf.list(token("protect")));

		// ExternalExpr ::= Identifier Name "defines" InstExpr
		tbl.put("ExternalDefines/3:ExternalExpr", vf.list(child(0), space(" "), child(1), space(" "), token("defines"), space(" "), child(2)));

		// ImportClause ::= "module" Name "=" Name
		tbl.put("ImportRename/2:ImportClause", vf.list(token("module"), space(" "), child(0), space(" "), token("="), space(" "), child(1)));

		// Expr ::= LGNOT Expr
		tbl.put("PreOp/2:Expr", vf.list(child(0), space(" "), child(1)));

		// SingleAlgDecl ::= Modifier* StatDeclarative SubClause*
		tbl.put("StatDecl/3:SingleAlgDecl", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// Literal ::= DecNumeral
		tbl.put("Int/1:Literal", vf.list(child(0)));

		// Expr ::= "(" {Expr ","}* ")"
		tbl.put("Tuple/1:Expr", vf.list(token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// Expr ::= "if" Expr "then" Expr "else" Expr "end"
		tbl.put("IfThenElseExpr/3:Expr", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("else"), space(" "), child(2), space(" "), token("end")));

		// Stat ::= "open" {Identifier ","}* "in" Stat* "end"
		tbl.put("Open/2:Stat", vf.list(token("open"), space(" "), sep(child(0),","), space(" "), token("in"), space(" "), child(1), space(" "), token("end")));

		return tbl;
	}
}
