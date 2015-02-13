package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcomputedInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCcomputedInvocation  extends ASTinvocation
implements IASTCcomputedInvocation {
    
    public ASTCcomputedInvocation (IASTexpression function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
