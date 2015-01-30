package com.paracamplus.ilp9.interfaces;

public interface IASTbinaryOperation extends IASToperation {
	IASTexpression getLeftOperand();
	IASTexpression getRightOperand();
}
