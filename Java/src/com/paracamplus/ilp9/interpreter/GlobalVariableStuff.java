package com.paracamplus.ilp9.interpreter;

import java.math.BigDecimal;

import com.paracamplus.ilp9.interpreter.primitive.Newline;
import com.paracamplus.ilp9.interpreter.primitive.Print;

public class GlobalVariableStuff {
    
    public static void fillGlobalVariables (IGlobalVariableEnvironment env) {
        env.addGlobalVariableValue("pi",
                new BigDecimal("3.1415926535"));
        env.addGlobalVariableValue("print",
                new Print());
        env.addGlobalVariableValue("newline",
                new Newline());
    }
  }
