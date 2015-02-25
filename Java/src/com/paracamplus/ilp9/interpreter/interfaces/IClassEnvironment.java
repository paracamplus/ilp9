package com.paracamplus.ilp9.interpreter.interfaces;

public interface IClassEnvironment {
    IClass getILP9Class(String name) throws EvaluationException;
    void addILP9Class(IClass clazz);
}
