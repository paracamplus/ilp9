package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTvariable;

public class ASTLocalFunctionVariable extends ASTvariable
implements IASTLocalFunctionVariable {

    public ASTLocalFunctionVariable (String name) {
        super(name);
    }
}
