package com.paracamplus.ilp9.interfaces;

public interface IASTwriteField extends IASTexpression {
	 IASTexpression getObject();
	 String getFieldName();
	 IASTexpression getValue();
}
