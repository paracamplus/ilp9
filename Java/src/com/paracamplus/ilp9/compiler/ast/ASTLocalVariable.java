package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTvariable;

public class ASTLocalVariable extends ASTvariable 
implements IASTLocalVariable {

    public ASTLocalVariable (String name) {
        super(name);
    }
}
