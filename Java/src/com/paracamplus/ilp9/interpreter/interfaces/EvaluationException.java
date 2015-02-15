package com.paracamplus.ilp9.interpreter;

@SuppressWarnings("serial")
public class EvaluationException extends Exception {

    public EvaluationException(String msg) {
        super(msg);
    }

    public EvaluationException(Exception e) {
        super(e);
    }
}
