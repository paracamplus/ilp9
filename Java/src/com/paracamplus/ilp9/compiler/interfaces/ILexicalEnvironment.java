package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.tools.IEnvironment;

public interface ILexicalEnvironment 
extends IEnvironment<IASTvariable, Object, CompilationException> {  

    // Void FIXME ???
    
    // A touch of covariance:
    ILexicalEnvironment extend(IASTvariable variable, Object value);
    ILexicalEnvironment getNext() throws CompilationException;
}
