package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTvisitor;

public interface IASTCvisitor<Result, Data, Anomaly extends Throwable> 
extends IASTvisitor<Result, Data, Anomaly> {
    Result visit(IASTCcodefinitions iast, Data data) throws Anomaly;
    Result visit(IASTCcomputedInvocation iast, Data data) throws Anomaly;
    Result visit(IASTCglobalFunctionVariable iast, Data data) throws Anomaly;
    Result visit(IASTCglobalInvocation iast, Data data) throws Anomaly;
    Result visit(IASTCglobalVariable iast, Data data) throws Anomaly;
    Result visit(IASTClambda iast, Data data) throws Anomaly;
    Result visit(IASTClocalFunctionInvocation iast, Data data) throws Anomaly;
    Result visit(IASTClocalFunctionVariable iast, Data data) throws Anomaly;
    Result visit(IASTClocalVariable iast, Data data) throws Anomaly;
    Result visit(IASTCprimitiveInvocation iast, Data data) throws Anomaly;
    Result visit(IASTCvariable iast, Data data) throws Anomaly;
    Result visit(IASTCinstantiation iast, Data data) throws Anomaly;
    Result visit(IASTCfieldRead iast, Data data) throws Anomaly;
    Result visit(IASTCfieldWrite iast, Data data) throws Anomaly;
}
