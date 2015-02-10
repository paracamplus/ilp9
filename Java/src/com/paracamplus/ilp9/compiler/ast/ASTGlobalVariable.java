package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTvariable;

public class ASTGlobalVariable extends ASTvariable 
implements IASTGlobalVariable {

    public ASTGlobalVariable (String name) {
        super(name);
    }
}
