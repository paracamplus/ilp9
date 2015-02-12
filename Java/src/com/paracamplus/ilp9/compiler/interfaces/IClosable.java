package com.paracamplus.ilp9.compiler.interfaces;

import java.util.Set;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface IClosable {
    Set<IASTvariable> getClosedVariables();
    void setClosedVariables(Set<IASTvariable> newvars);
}
