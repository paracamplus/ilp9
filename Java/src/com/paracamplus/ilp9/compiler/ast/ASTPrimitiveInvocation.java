package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTPrimitiveInvocation extends ASTinvocation
implements IASTPrimitiveInvocation {
    
    public ASTPrimitiveInvocation(IASTvariable function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
