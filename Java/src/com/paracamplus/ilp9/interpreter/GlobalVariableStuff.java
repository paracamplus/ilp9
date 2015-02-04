package com.paracamplus.ilp9.interpreter;

import java.math.BigDecimal;

import com.paracamplus.ilp9.ast.ASTvariable;

public class GlobalVariableStuff {
    
    public static void fillGlobalVariables (IGlobalVariableEnvironment env) {
        env.addGlobalVariableValue(
                new ASTvariable("pi"),
                new BigDecimal("3.1415926535"));
        // TODO print, newline
    }
  }
