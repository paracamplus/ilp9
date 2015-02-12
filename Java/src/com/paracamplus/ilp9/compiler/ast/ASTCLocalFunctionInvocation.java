package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCLocalFunctionInvocation extends ASTinvocation
implements IASTCLocalFunctionInvocation {

    public ASTCLocalFunctionInvocation (IASTexpression function, 
                                       IASTexpression[] arguments) {
        super(function, arguments);
    }
}
