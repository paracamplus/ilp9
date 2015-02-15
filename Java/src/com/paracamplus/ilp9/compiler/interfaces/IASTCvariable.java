package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public abstract interface IASTCvariable 
extends IASTvariable, IASTCvisitable {
    boolean isMutable();
    void setMutable();
}
