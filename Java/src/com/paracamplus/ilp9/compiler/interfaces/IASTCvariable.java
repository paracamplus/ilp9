package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface IASTCvariable extends IASTvariable {
    boolean isMutable();
    void setMutable();
}
