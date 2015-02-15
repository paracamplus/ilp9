package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IAST;

public abstract class AST implements IAST {
    public String getClassShortName() {
        return this.getClass().getName()
                .replaceFirst("^com.paracamplus.ilp9.", "");
    }
}
