package com.paracamplus.ilp9.compiler.normalizer;

import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface INormalizationEnvironment {
    INormalizationEnvironment extend(IASTvariable oldv, IASTvariable newv);
    IASTvariable renaming(IASTvariable variable) 
            throws NoSuchLocalVariableException;
}
