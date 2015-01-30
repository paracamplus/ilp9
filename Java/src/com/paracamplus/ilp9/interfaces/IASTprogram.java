package com.paracamplus.ilp9.interfaces;

import java.util.Map;

public interface IASTprogram extends IAST {
	IASTexpression[] getBody();
	Map<String, IASTfunctionDefinition> getFunctionDefinitions();
	Map<String, IASTvariable> getVariables();
	Map<String, IASTclassDefinition> getClasses();
}
