package com.paracamplus.ilp9.interpreter;

public interface IGlobalVariableEnvironment {
    Object getGlobalVariableValue (String variableName);
    void addGlobalVariableValue (String variableName, Object value);
    void updateGlobalVariableValue (String variableName, Object value);
}
