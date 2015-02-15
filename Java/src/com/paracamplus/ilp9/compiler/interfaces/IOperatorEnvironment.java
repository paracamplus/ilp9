package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.interfaces.IASToperator;

public interface IOperatorEnvironment {
    String getUnaryOperator (IASToperator operator) 
            throws CompilationException;
    String getBinaryOperator (IASToperator operator) 
            throws CompilationException;
    void addUnaryOperator (String operator, String cOperator)
            throws CompilationException;
    void addBinaryOperator (String operator, String cOperator) 
            throws CompilationException;

}
