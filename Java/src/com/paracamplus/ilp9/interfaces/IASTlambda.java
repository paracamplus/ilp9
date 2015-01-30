package com.paracamplus.ilp9.interfaces;

public interface IASTlambda extends IASTexpression {
	IASTvariable[] getVariables();
	IASTexpression getBody();
}
