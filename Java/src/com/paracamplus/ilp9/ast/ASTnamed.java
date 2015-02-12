package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.Inamed;

public abstract class ASTnamed extends AST implements Inamed {
    
    public ASTnamed (String name) {
        this.name = name;
    }
    private String name;
    
    public String getName() {
        return name;
    }
}
