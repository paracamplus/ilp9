package com.paracamplus.ilp9.compiler.ast;

import java.util.concurrent.atomic.AtomicInteger;

import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTCnamedLambda extends ASTClambda 
implements IASTCnamedLambda {

    public ASTCnamedLambda (IASTvariable functionVariable,
                            IASTvariable[] variables, 
                            IASTexpression body) {
        super(functionVariable.getName() + "_" + counter.incrementAndGet(), 
              variables, body);
        this.functionVariable = functionVariable;
    }
    private final IASTvariable functionVariable;
    private static final AtomicInteger counter = new AtomicInteger(0);
    
    public IASTvariable getFunctionVariable() {
        return functionVariable;
    }
}
