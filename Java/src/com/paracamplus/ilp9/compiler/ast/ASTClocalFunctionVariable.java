package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;

public class ASTClocalFunctionVariable extends ASTClocalVariable
implements IASTClocalFunctionVariable {

    public ASTClocalFunctionVariable (String name) {
        super(name);
    }
}
