package com.paracamplus.ilp9.interpreter.primitive;

import java.io.IOException;
import java.io.Writer;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;

public class Newline extends Primitive {
    
    public Newline(Writer out) {
        super("newline");
        this.out = out;
    }
    private final Writer out;
    
    public int getArity () {
        return 0;
    }
    
    public Object apply (Object value) throws EvaluationException {
        try {
            out.append("\n");
        } catch (IOException e) {
            throw new EvaluationException(e);
        }
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
