package com.paracamplus.ilp9.interpreter.interfaces;

import com.paracamplus.ilp9.interpreter.Interpreter;

public interface Invocable {
    int getArity();
    Object apply(Interpreter interpreter, Object[] argument) 
            throws EvaluationException;
}
