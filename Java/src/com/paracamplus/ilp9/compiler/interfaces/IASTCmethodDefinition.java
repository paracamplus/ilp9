package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;

public interface IASTCmethodDefinition 
extends IASTmethodDefinition,
        IASTCfunctionDefinition,
        IASTCclassRelated {
    IASTCmethodDefinition findSuperMethod() throws CompilationException;
}
