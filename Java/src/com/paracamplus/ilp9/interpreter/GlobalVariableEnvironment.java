package com.paracamplus.ilp9.interpreter;

import java.util.HashMap;
import java.util.Map;

public class GlobalVariableEnvironment implements IGlobalVariableEnvironment {

    public GlobalVariableEnvironment () {
        this.globalVariableEnvironment = new HashMap<>();
    }
    private final Map<String, Object> globalVariableEnvironment;
    
    public Object getGlobalVariableValue(String variableName) {
        Object value = globalVariableEnvironment.get(variableName);
        return value;
    }

    public void addGlobalVariableValue(String variableName, Object value) {
        globalVariableEnvironment.put(variableName, value);
    }
    
    public void updateGlobalVariableValue(String variableName, Object value) {
        globalVariableEnvironment.put(variableName, value);
    }
}
