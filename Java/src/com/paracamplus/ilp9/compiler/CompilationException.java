package com.paracamplus.ilp9.compiler;

@SuppressWarnings("serial")
public class CompilationException extends Exception {

    public CompilationException(String msg) {
        super(msg);
    }

    public CompilationException(Exception e) {
        super(e);
    }
}
