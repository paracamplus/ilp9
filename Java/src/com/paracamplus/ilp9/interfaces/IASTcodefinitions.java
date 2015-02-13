package com.paracamplus.ilp9.interfaces;

public interface IASTcodefinitions extends IASTexpression {
    IASTnamedLambda[] getFunctions();
    IASTexpression getBody();
}
