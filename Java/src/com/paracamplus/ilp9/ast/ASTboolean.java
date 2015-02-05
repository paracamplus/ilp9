package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTboolean extends ASTconstant implements IASTboolean {

    public ASTboolean (String description) {
        super(description, "true".equals(description));
    }
    
    public Boolean getValue() {
        return (Boolean) super.getValue();
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
