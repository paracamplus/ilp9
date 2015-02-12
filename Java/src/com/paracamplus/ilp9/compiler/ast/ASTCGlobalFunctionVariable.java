package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalFunctionVariable;

public class ASTCGlobalFunctionVariable extends ASTCGlobalVariable
implements IASTCGlobalFunctionVariable {

    public ASTCGlobalFunctionVariable (String name) {
        super(name);
    }
}
