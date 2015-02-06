package com.paracamplus.ilp9.compiler;

import java.util.HashMap;
import java.util.Map;

public class GlobalVariableEnvironment 
implements IGlobalVariableEnvironment {

    public GlobalVariableEnvironment () {
        this.globalVariableEnvironment = new HashMap<>();
    }
    private final Map<String, String> globalVariableEnvironment;

    public void addGlobalVariableValue(String variableName, String cName) {
        globalVariableEnvironment.put(variableName, cName);
    }

    public void addGlobalVariableValue(IPrimitive primitive) {
        globalVariableEnvironment.put(primitive.getName(), 
                                      primitive.getCName());
    }
}
