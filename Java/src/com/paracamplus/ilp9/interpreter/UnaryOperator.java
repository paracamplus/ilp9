package com.paracamplus.ilp9.interpreter;

public abstract class UnaryOperator extends Operator {
    
    public UnaryOperator (String name) {
        super(name);
    }

    public int getArity() {
        return 1;
    }
    
    public abstract Object apply (Object operand) throws EvaluationException;

    public Object apply(Object... argument) throws EvaluationException {
        if ( argument.length == getArity() ) {
            return apply(argument[0]);
        } else {
            String msg = "Wrong arity for operator " + this.getName();
            throw new EvaluationException(msg);
        }
    }
}
