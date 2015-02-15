package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTvisitable;

public interface IASTCvisitable extends IASTvisitable {
    <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTCvisitor<Result, Data, Anomaly> visitor,
                  Data data) throws Anomaly;
}
