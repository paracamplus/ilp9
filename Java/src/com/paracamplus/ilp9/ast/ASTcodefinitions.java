package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTcodefinitions;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTcodefinitions extends ASTexpression 
implements IASTcodefinitions {

    public ASTcodefinitions (IASTfunctionDefinition[] functions, 
                             IASTexpression body ) {
        this.functions = functions;
        this.body = body;
    }
    private final IASTfunctionDefinition[] functions;
    private final IASTexpression body;
    
    public IASTfunctionDefinition[] getFunctions() {
        return functions;
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
