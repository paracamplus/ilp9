package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTnamedLambda extends ASTlambda implements IASTnamedLambda {

    public ASTnamedLambda (IASTvariable functionVariable,
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
