module MagnoliaCxxBack
import Magnolia;
import MagnoliaCompile;
import XaTree;

public XaTree cxxFormat(XaTree tree) {
	return visit(tree) {
		case d: cons("Param", [ID, TYPE], s) => d[@concrete=[token("const"), space(" "), child(1), token("&"), space(" "), child(0)]]
		case d: cons("FunClause", [NAME, ARGS, TYPE], s) =>
				d[@concrete = [child(2), space(" "), child(0), child(1)]]
//		case d: cons("PredClause", [NAME, ARGS], s) =>
//				d[@concrete = [token("predicate"), space(" "), child(0), token("("), child(1), token(")")]]
//		case d: cons("AxiomClause", [NAME, ARGS], s) =>
//				d[@concrete = [token("axiom"), space(" "), child(0), token("("), child(1), token(")")]]
		case d: cons("ProcClause", [NAME, ARGS], s) =>
				d[@concrete = [token("void"), space(" "), child(0), child(1)]]
		case d: cons("TypeClause", [NAME], s) =>
				d[@concrete = [token("struct"), space(" "), child(0)]]
		case d: cons("VarClause", [NAME, TYPE], s) =>
				d[@concrete = [child(1), space(" "), child(0)]]
		case n: cons("Name",[leaf(op, "ID")], "Identifier"):
				if([opname] := getOpName(op))
					insert n[@concrete = [token("<opname>")]];
				else
					fail;		
	}
}

public list[str] getOpName(str op) {
	if(/_<opname:[^a-zA-Z]+>_/ :=op)
		return ["operator<opname>"];
	else
		return [];
}