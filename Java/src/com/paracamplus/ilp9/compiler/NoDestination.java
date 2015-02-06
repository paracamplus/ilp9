package com.paracamplus.ilp9.compiler;

public class NoDestination implements IDestination {

    public static final NoDestination NO_DESTINATION = 
            new NoDestination();
    
    private NoDestination () {}
    
    public String compile() {
        return "";
    }
}
