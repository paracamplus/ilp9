package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalVariable;

public class ASTCLocalVariable extends ASTCvariable 
implements IASTCLocalVariable {

    public ASTCLocalVariable (String name) {
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
