package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTblock;

public interface IASTCblock extends IASTblock {

    interface IASTCbinding extends IASTblock.IASTbinding {
        // Covariance
        IASTClocalVariable getVariable();
    }

    IASTCbinding[] getBindings();
}
