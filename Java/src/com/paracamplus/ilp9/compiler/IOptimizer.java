package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.compiler.ast.IASTCprogram;

public interface IOptimizer {
        IASTCprogram transform (IASTCprogram program) 
                throws CompilationException;
}
