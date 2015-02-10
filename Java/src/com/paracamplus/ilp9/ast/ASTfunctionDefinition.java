package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTvariable;

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
}
