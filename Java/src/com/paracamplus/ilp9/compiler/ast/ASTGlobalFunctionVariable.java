package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTvariable;

public class ASTGlobalFunctionVariable extends ASTvariable
implements IASTGlobalFunctionVariable {

    public ASTGlobalFunctionVariable (String name) {
        super(name);
    }
}
