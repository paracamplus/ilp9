package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTstring extends ASTconstant implements IASTstring {

    public ASTstring (String description) {
        super(description, description);
    }
    public String getValue() {
        return (String) super.getValue();
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
