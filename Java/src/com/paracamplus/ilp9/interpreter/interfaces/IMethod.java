package com.paracamplus.ilp9.interpreter.interfaces;

public interface IMethod extends Invocable {
    String getName();
    IClass getDefiningClass();
    void setDefiningClass(IClass clazz);
    int getMethodArity();
}
