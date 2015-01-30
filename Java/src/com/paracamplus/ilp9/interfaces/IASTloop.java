package com.paracamplus.ilp9.interfaces;

public interface IASTloop extends IASTinstruction {
	IASTexpression getCondition();
	IASTexpression getBody();
}
