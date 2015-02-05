package com.paracamplus.ilp9.interpreter.primitive;

import com.paracamplus.ilp9.interpreter.EvaluationException;
import com.paracamplus.ilp9.interpreter.Primitive;

public abstract class UnaryPrimitive extends Primitive {
    
    public UnaryPrimitive(String name) {
        super(name);
    }

    public int getArity () {
        return 1;
    }
    
    public abstract Object apply(Object arg1) throws EvaluationException; 
}
