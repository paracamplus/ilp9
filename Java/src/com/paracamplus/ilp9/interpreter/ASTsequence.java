package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTsequence;

public class ASTsequence extends ASTexpression implements IASTsequence {
    public ASTsequence (IASTexpression[] expressions) {
        this.expressions = expressions;
    }
    protected IASTexpression[] expressions;
    
    public IASTexpression[] getExpressions() {
        return this.expressions;
    }
}
