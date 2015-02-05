package com.paracamplus.ilp9.interpreter.primitive;

import com.paracamplus.ilp9.interpreter.EvaluationException;
import com.paracamplus.ilp9.interpreter.Primitive;

public class Newline extends Primitive {
    
    public Newline() {
        super("newline");
    }
    
    public int getArity () {
        return 0;
    }
    
    public Object apply (Object value) {
        System.out.println("");
        return Boolean.FALSE;
    }
    
    public Object apply(Object... argument) throws EvaluationException {
        if ( argument.length == getArity() ) {
            return apply();
        } else {
            String msg = "Wrong arity for operator " + this.getName();
            throw new EvaluationException(msg);
        }
    }

}
