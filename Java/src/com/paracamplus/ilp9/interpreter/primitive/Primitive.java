package com.paracamplus.ilp9.interpreter;

public abstract class Primitive implements IPrimitive {

    public Primitive (String name) {
        this.name = name;
    }
    private final String name;
    
    public String getName() {
        return name;
    }
}
