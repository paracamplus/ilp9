package com.paracamplus.ilp9.interfaces;

interface IASTBinding extends IAST {
	IASTvariable getVariable();
	IASTexpression getInitialization();
}

public interface IASTblock extends IASTexpression {
	IASTBinding[] getBindings();
	IASTexpression getBody();
}
