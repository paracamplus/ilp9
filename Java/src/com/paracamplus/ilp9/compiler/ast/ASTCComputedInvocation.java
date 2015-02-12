package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCComputedInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCComputedInvocation  extends ASTinvocation
implements IASTCComputedInvocation {
    
    public ASTCComputedInvocation (IASTexpression function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
