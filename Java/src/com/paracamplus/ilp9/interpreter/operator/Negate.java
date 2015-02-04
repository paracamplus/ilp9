package com.paracamplus.ilp9.interpreter.operator;

import com.paracamplus.ilp9.interpreter.EvaluationException;
import com.paracamplus.ilp9.interpreter.UnaryOperator;

public class Negate extends UnaryOperator {
    
    public Negate () {
        super("!");
    }
    
    public Object apply (Object operand) throws EvaluationException {
        if ( operand instanceof Boolean ) {
            Boolean result = ! ((Boolean) operand);
            return result;
        } else {
            return Boolean.FALSE;
        }
    }
}
