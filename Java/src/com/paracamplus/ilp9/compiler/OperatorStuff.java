package com.paracamplus.ilp9.compiler;

public class OperatorStuff {

    public static void fillUnaryOperators (IOperatorEnvironment env)
            throws CompilationException {
        env.addUnaryOperator("-", "ILP_Opposite");
        env.addUnaryOperator("!", "ILP_Not");
    }

    public static void fillBinaryOperators (IOperatorEnvironment env) 
            throws CompilationException {
        env.addBinaryOperator("+", "ILP_Plus");
        env.addBinaryOperator("*", "ILP_Times");
        env.addBinaryOperator("/", "ILP_Divide");
        env.addBinaryOperator("%", "ILP_Modulo");
        env.addBinaryOperator("-", "ILP_Minus");
        // comparators
        env.addBinaryOperator("<", "ILP_LessThan");
        env.addBinaryOperator("<=", "ILP_LessThanOrEqual");
        env.addBinaryOperator("==", "ILP_Equal");
        env.addBinaryOperator("!=", "ILP_NotEqual"); 
        env.addBinaryOperator("<>", "ILP_NotEqual");
        env.addBinaryOperator(">", "ILP_GreaterThan");
        env.addBinaryOperator(">=", "ILP_GreaterThanOrEqual");
        //
        env.addBinaryOperator("&&", "ILP_And");
        env.addBinaryOperator("||", "ILP_Or");
        env.addBinaryOperator("^", "ILP_Xor");
    }
}
