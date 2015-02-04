package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTconstant;

public abstract class ASTconstant extends ASTexpression 
implements IASTconstant {

    public ASTconstant(String description, Object value) {
        this.description = description;
        this.value = value;
    }
    private final String description;
    private final Object value;
    
    public String getDescription() {
        return this.description;
    }

    public Object getValue() {
        return this.value;
    }
}
