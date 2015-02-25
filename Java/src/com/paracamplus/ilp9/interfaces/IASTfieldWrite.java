package com.paracamplus.ilp9.interfaces;

public interface IASTfieldWrite extends IASTexpression {
	 IASTexpression getTarget();
	 String getFieldName();
	 IASTexpression getValue();
}
