package com.paracamplus.ilp9.interpreter.interfaces;

public interface ISuperCallInformation {
    Object[] getArguments();
    IMethod getSuperMethod() throws EvaluationException;
}
