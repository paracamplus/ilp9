package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public class AssignDestination implements IDestination {

    // FIXME
    
    public AssignDestination (IASTvariable variable) {
        this.variable = variable;
    }
    private final IASTvariable variable;
    
    public String compile() {
        return variable.getMangledName() + " = ";
    }

}
