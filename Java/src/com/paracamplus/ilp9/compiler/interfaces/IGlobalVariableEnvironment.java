package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTvariable;


public interface IGlobalVariableEnvironment {
    void addGlobalVariableValue (String variableName, String cName);
    void addGlobalFunctionValue (IPrimitive primitive);
    boolean isPrimitive(IASTvariable variable);
    IPrimitive getPrimitiveDescription(IASTvariable variable);
    String getCName (IASTvariable variable);
}
