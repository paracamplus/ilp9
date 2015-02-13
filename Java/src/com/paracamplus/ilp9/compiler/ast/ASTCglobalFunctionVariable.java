package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;

public class ASTCglobalFunctionVariable extends ASTCglobalVariable
implements IASTCglobalFunctionVariable {

    public ASTCglobalFunctionVariable (String name) {
        super(name);
    }
}
