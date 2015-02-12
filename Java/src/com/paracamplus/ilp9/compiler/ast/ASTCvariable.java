package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTvariable;

public class ASTCvariable extends ASTvariable {

    public ASTCvariable(String name) {
        super(name);
        this.mutable = false;
    }
    private boolean mutable;

    public boolean isMutable() {
        return mutable;
    }

    public void setMutable() {
        mutable = true;
    }
}
