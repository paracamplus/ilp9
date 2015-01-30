package com.paracamplus.ilp9.interfaces;

import com.paracamplus.ilp9.annotation.OrNull;

public interface IASTtry extends IASTinstruction {
    IASTexpression getBody ();
    @OrNull IASTvariable getCaughtExceptionVariable ();
    @OrNull IASTexpression getCatcher ();
    @OrNull IASTexpression getFinallyer ();
}
