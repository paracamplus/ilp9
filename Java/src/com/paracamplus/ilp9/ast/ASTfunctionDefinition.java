package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTfunctionDefinition extends ASTnamed 
implements IASTfunctionDefinition {
    
    public ASTfunctionDefinition (String functionName,
                                  IASTvariable[] variables,
                                  IASTexpression body ) {
        super(functionName);
        this.variables = variables;
        this.body = body;
    }
    private final IASTvariable[] variables;
    private final IASTexpression body;

    public IASTvariable[] getVariables() {
        return variables;
    }

    public IASTexpression getBody() {
        return body;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
