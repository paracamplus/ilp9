package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCPrimitiveInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTCPrimitiveInvocation extends ASTinvocation
implements IASTCPrimitiveInvocation {
    
    public ASTCPrimitiveInvocation(IASTvariable function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
