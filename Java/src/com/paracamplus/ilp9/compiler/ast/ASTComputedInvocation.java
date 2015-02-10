package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTComputedInvocation  extends ASTinvocation
implements IASTComputedInvocation {
    
    public ASTComputedInvocation (IASTexpression function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
