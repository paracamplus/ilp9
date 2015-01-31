package com.paracamplus.ilp9.interfaces;

import com.paracamplus.ilp9.annotation.OrNull;

public interface IASTalternative extends IASTexpression {
	IASTexpression getCondition();
	IASTexpression getConsequent();
	@OrNull IASTexpression getAlternant();
	boolean isTernary();
}
