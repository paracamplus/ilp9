package com.paracamplus.ilp9.interpreter;

@SuppressWarnings("serial")
public class ASTexception extends Exception {
    public ASTexception (final Throwable cause) {
        super(cause);
    }

    public ASTexception (final String message) {
        super(message);
    }
}
