package com.paracamplus.ilp9.interfaces;

public interface IASTprogram extends IAST {
	IASTexpression getBody();
	IASTfunctionDefinition[] getFunctionDefinitions();
	IASTclassDefinition[] getClassDefinitions();
}
