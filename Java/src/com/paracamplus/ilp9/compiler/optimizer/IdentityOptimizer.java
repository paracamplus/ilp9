package com.paracamplus.ilp9.compiler.optimizer;

import com.paracamplus.ilp9.compiler.IOptimizer;
import com.paracamplus.ilp9.compiler.ast.IASTCprogram;

public class IdentityOptimizer implements IOptimizer {

    public IASTCprogram transform(IASTCprogram program) {
        return program;
    }
}
