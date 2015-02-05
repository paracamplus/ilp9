package com.paracamplus.ilp9.interfaces;

import java.util.Map;

public interface IASTprogram extends IAST {
	IASTexpression getBody();
	IASTfunctionDefinition[] getFunctionDefinitions();
	Map<String, IASTclassDefinition> getClassDefinitions();
}
