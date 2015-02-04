package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASToperator;

public interface IOperatorEnvironment {
    IOperator getUnaryOperator (IASToperator operator) 
            throws EvaluationException;
    IOperator getBinaryOperator (IASToperator operator) 
            throws EvaluationException;
    void addOperator (IOperator operator) throws EvaluationException;
}
