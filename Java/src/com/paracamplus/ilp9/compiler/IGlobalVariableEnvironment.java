package com.paracamplus.ilp9.compiler;


public interface IGlobalVariableEnvironment {
    void addGlobalVariableValue (String variableName, String cName);
    void addGlobalVariableValue (IPrimitive primitive);

}
