package com.paracamplus.ilp9.compiler;

public class ReturnDestination implements IDestination {
    
    private ReturnDestination () {}
    
    public static final ReturnDestination RETURN_DESTINATION =
            new  ReturnDestination();

    public String compile() {
        return "return ";
    }
}
