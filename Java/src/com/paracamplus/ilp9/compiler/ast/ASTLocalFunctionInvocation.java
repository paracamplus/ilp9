package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTLocalFunctionInvocation extends ASTinvocation
implements IASTLocalFunctionInvocation {

    public ASTLocalFunctionInvocation (IASTexpression function, 
                                       IASTexpression[] arguments) {
        super(function, arguments);
    }
}
