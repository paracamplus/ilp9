package com.paracamplus.ilp9.interpreter;

import java.util.HashMap;
import java.util.Map;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public class GlobalVariableEnvironment implements IGlobalVariableEnvironment {

    public GlobalVariableEnvironment () {
        this.globalVariableEnvironment = new HashMap<>();
    }
    private final Map<String, Object> globalVariableEnvironment;
    
    public Object getGlobalVariableValue(IASTvariable variable) {
        Object value = globalVariableEnvironment.get(variable.getName());
        return value;
    }

    public void addGlobalVariableValue(IASTvariable variable, Object value) {
        globalVariableEnvironment.put(variable.getName(), value);
    }
    
    public void updateGlobalVariableValue(IASTvariable variable, Object value) {
        globalVariableEnvironment.put(variable.getName(), value);
    }
}
