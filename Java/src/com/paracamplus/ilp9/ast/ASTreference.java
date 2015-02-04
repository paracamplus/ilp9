package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTreference;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTreference extends ASTexpression implements IASTreference {
    
    public ASTreference (IASTvariable variable) {
        this.variable = variable;
    }
    private final IASTvariable variable;
    
    public IASTvariable getVariable() {
        return variable;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
