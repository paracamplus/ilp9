package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTGlobalInvocation extends ASTinvocation
implements IASTGlobalInvocation {
    
    public ASTGlobalInvocation (IASTexpression function, 
                                IASTexpression[] arguments) {
        super(function, arguments);
    }
    
    public IASTGlobalVariable getFunction() {
        return (IASTGlobalVariable) super.getFunction();
    }
}
