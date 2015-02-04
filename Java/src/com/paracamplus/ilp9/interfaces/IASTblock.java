package com.paracamplus.ilp9.interfaces;

public interface IASTblock extends IASTexpression {
    
    interface IASTbinding extends IAST {
        IASTvariable getVariable();
        IASTexpression getInitialisation();
    }

	IASTbinding[] getBindings();
	IASTexpression getBody();
}
