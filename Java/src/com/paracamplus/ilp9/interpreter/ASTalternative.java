package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.annotation.OrNull;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTalternative extends ASTexpression
implements IASTalternative, ItoXML, Ievaluable {

    public ASTalternative(IASTexpression condition,
                          IASTexpression consequence) {
        this(condition, consequence, null);
    }
    
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

	public IASTexpression getConsequent() {
		return consequence;
	}

	public IASTexpression getAlternant() {
		return alternant;
	}

	public boolean isTernary () {
	    return this.alternant != null;
	}

	public String toXML () {
	    StringBuffer sb = new StringBuffer();
	    sb.append("<alternative><condition>");
	    sb.append(getCondition().toXML());
	    sb.append("</condition><consequence>");
	    sb.append(getConsequent().toXML());
	    sb.append("</consequence>");
	    if ( isTernary() ) {
	      sb.append("<alternant>");
	      sb.append(getAlternant().toXML());
	      sb.append("</alternant>");
	    }
	    sb.append("</alternative>");
	    return sb.toString();
	}

	public Object eval (final ILexicalEnvironment lexenv,
	                    final ICommon common)
	                            throws EvaluationException {
	    Object bool = getCondition().eval(lexenv, common);
	    if ( bool instanceof Boolean ) {
	        Boolean b = (Boolean) bool;
	        if ( b.booleanValue() ) {
	            return getConsequent().eval(lexenv, common);
	        } else {
	            return getAlternant().eval(lexenv, common);
	        }
	    } else {
	        return getConsequent().eval(lexenv, common);
	    }
	}
}
