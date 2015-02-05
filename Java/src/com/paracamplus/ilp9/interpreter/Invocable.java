package com.paracamplus.ilp9.interpreter;

public interface Invocable {
    int getArity();
    Object apply(Interpreter interpreter, Object[] argument) 
            throws EvaluationException;
}
