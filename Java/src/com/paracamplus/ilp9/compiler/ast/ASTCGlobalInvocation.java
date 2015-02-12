package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCGlobalInvocation extends ASTinvocation
implements IASTCGlobalInvocation {
    
    public ASTCGlobalInvocation (IASTexpression function, 
                                IASTexpression[] arguments) {
        super(function, arguments);
    }
    
    public IASTCGlobalVariable getFunction() {
        return (IASTCGlobalVariable) super.getFunction();
    }
}
