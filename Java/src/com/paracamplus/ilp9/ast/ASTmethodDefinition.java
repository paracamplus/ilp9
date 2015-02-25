package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTmethodDefinition extends ASTfunctionDefinition
implements IASTmethodDefinition {
    
    public ASTmethodDefinition(IASTvariable methodVariable, 
                               IASTvariable[] variables,
                               IASTexpression body,
                               String definingClassName ) {
        super(methodVariable, variables, body);
        this.definingClassName = definingClassName;
    }
    private final String definingClassName;    

    public String getDefiningClassName() {
        return definingClassName;
    }
    
    public String getMethodName() {
        return getFunctionVariable().getName();
    }
}
