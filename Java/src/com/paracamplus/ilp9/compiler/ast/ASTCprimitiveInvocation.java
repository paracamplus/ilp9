package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprimitiveInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTCprimitiveInvocation extends ASTinvocation
implements IASTCprimitiveInvocation {
    
    public ASTCprimitiveInvocation(IASTvariable function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }

    public <Result, Data, Anomaly extends Throwable> Result 
        accept(IASTCvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
