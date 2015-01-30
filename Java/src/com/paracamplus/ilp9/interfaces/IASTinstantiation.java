package com.paracamplus.ilp9.interfaces;

public interface IASTinstantiation extends IASTexpression {
	String getClassName();
	IASTexpression[] getArguments();
}
