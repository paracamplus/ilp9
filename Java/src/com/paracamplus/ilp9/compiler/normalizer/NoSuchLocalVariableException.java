package com.paracamplus.ilp9.compiler.normalizer;

import com.paracamplus.ilp9.compiler.CompilationException;

@SuppressWarnings("serial")
public class NoSuchLocalVariableException extends CompilationException {

    public NoSuchLocalVariableException(String msg) {
        super(msg);
    }

    public NoSuchLocalVariableException(Exception e) {
        super(e);
    }
}
