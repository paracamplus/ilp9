package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionVariable;

public class ASTCLocalFunctionVariable extends ASTCLocalVariable
implements IASTCLocalFunctionVariable {

    public ASTCLocalFunctionVariable (String name) {
        super(name);
    }
}
