package com.paracamplus.ilp9.compiler;

public class GlobalVariableStuff {
    public static void fillGlobalVariables (IGlobalVariableEnvironment env) {
        env.addGlobalVariableValue("pi", "ILP_PI");
        env.addGlobalVariableValue("print", "ILP_print");
        env.addGlobalVariableValue("newline", "ILP_newline");
        env.addGlobalVariableValue("throw", "ILP_throw");
    }
}
