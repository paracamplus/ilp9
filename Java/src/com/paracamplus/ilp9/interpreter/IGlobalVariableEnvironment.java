package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface IGlobalVariableEnvironment {
    Object getGlobalVariableValue (IASTvariable variable);
    void addGlobalVariableValue (IASTvariable variable, Object value);
    void updateGlobalVariableValue (IASTvariable variable, Object value);
}
