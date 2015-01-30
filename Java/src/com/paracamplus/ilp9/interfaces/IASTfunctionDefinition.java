package com.paracamplus.ilp9.interfaces;

public interface IASTfunctionDefinition extends IASTdeclaration, Inamed {
	IASTvariable[] getVariables();
	IASTexpression getBody();
}
