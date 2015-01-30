package com.paracamplus.ilp9.interfaces;

public abstract interface IASToperation extends IASTexpression {
	IASToperator getOperator();
	IASTexpression[] getOperands();
}
