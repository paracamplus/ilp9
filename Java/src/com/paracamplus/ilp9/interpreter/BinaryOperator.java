package com.paracamplus.ilp9.interpreter;

public abstract class BinaryOperator extends Operator {
    
    public BinaryOperator (String name) {
        super(name);
    }
    
    public int getArity() {
        return 2;
    }
    
    public abstract Object apply (Object leftOperand, Object rightOperand) 
            throws EvaluationException;

    public Object apply(Object... argument) throws EvaluationException {
        if ( argument.length == getArity() ) {
            return apply(argument[0], argument[1]);
        } else {
            String msg = "Wrong arity for operator " + this.getName();
            throw new EvaluationException(msg);
        }
    }
}
