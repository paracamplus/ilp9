package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.compiler.interfaces.IDestination;

public class ReturnDestination implements IDestination {
    
    private ReturnDestination () {}
    
    public static final ReturnDestination RETURN_DESTINATION =
            new  ReturnDestination();

    public String compile() {
        return "return ";
    }
}
