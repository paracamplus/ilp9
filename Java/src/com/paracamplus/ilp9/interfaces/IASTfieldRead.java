package com.paracamplus.ilp9.interfaces;

public interface IASTfieldRead extends IASTexpression {
	IASTexpression getTarget();
    String getFieldName();
}
