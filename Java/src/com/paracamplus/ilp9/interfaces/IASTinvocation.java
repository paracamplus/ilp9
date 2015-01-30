package com.paracamplus.ilp9.interfaces;

public interface IASTinvocation extends IASTexpression {
	IASTexpression getFunction();
	IASTexpression[] getArguments();
}
