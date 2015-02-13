package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTNamedLambda extends ASTlambda implements IASTnamedLambda {

    public ASTNamedLambda (IASTvariable functionVariable,
            IASTvariable[] variables, 
            IASTexpression body) {
        super(variables, body);
        this.functionVariable = functionVariable;
    }            
    private final IASTvariable functionVariable;
    
    public IASTvariable getFunctionVariable() {
        return functionVariable;
    }
}
