package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTinvocation extends ASTexpression implements IASTinvocation {
    
    public ASTinvocation (IASTexpression function, IASTexpression[] arguments) {
        this.function = function;
        this.arguments = arguments;
    }
    private final IASTexpression function;
    private final IASTexpression[] arguments;
    
    public IASTexpression getFunction () {
        return function;
    }
    public IASTexpression[] getArguments () {
        return arguments;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
