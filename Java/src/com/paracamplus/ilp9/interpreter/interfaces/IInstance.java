package com.paracamplus.ilp9.interpreter.interfaces;

import com.paracamplus.ilp9.interpreter.Interpreter;

public interface IInstance {
    IClass classOf();
    Object read (String fieldName) throws EvaluationException;
    Object write (String fieldName, Object value) throws EvaluationException;
    Object send (Interpreter interpreter, 
                 String message, 
                 Object[] arguments) throws EvaluationException;
}
