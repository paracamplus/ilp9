package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTself extends ASTvariable implements IASTself {

    public ASTself () {
        super("self");
    }

    public <Result, Data, Anomaly extends Throwable> Result 
        accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
                throws Anomaly {
        return visitor.visit(this, data);
    }
}
