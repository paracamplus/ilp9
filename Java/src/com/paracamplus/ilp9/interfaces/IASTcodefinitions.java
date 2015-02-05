package com.paracamplus.ilp9.interfaces;

public interface IASTcodefinitions extends IASTexpression {
    IASTfunctionDefinition[] getFunctions();
    IASTexpression getBody();
}
