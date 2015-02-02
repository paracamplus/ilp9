package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.annotation.OrNull;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTalternative extends ASTexpression
implements IASTalternative {
    
	public ASTalternative(IASTexpression condition,
                          IASTexpression consequence,
                          IASTexpression alternant ) {
		this.condition = condition;
		this.consequence = consequence;
		this.alternant = alternant;
	}
	private final IASTexpression condition;
	private final IASTexpression consequence;
	private @OrNull final IASTexpression alternant;

	public IASTexpression getCondition() {
		return condition;
	}

	public IASTexpression getConsequence() {
		return consequence;
	}

	public IASTexpression getAlternant() {
		return alternant;
	}

	public boolean isTernary () {
	    return this.alternant != null;
	}
}
