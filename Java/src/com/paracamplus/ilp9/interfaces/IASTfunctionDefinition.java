package com.paracamplus.ilp9.interfaces;

public interface IASTfunctionDefinition extends IASTdeclaration, Inamed {
    IASTvariable getFunctionVariable();
	IASTvariable[] getVariables();
	IASTexpression getBody();
}
