package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;

public class ASTClocalVariable extends ASTCvariable 
implements IASTClocalVariable {

    public ASTClocalVariable (String name) {
        super(name);
        this.closed = false;
    }
    private boolean closed;

    public boolean isClosed() {
        return closed;
    }

    public void setClosed() {
        closed = true;        
    }
}
