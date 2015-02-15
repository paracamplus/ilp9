package com.paracamplus.ilp9.compiler.optimizer;

import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IOptimizer;

public class IdentityOptimizer implements IOptimizer {

    public IASTCprogram transform(IASTCprogram program) {
        return program;
    }
}
