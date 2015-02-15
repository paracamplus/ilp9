package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.compiler.CompilationException;

public abstract interface IOptimizer {
        IASTCprogram transform (IASTCprogram program) 
                throws CompilationException;
}
