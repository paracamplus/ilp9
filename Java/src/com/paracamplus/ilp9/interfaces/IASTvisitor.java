package com.paracamplus.ilp9.interfaces;

public interface IASTvisitor<Result, Data, Anomaly extends Throwable> {
    Result visit(IASTalternative iast, Data data) throws Anomaly;
    Result visit(IASTassignment iast, Data data) throws Anomaly;
    Result visit(IASTbinaryOperation iast, Data data) throws Anomaly;
    Result visit(IASTblock iast, Data data) throws Anomaly;
    Result visit(IASTboolean iast, Data data) throws Anomaly;
    Result visit(IASTcodefinitions iast, Data data) throws Anomaly;
    Result visit(IASTfloat iast, Data data) throws Anomaly;
    Result visit(IASTinstantiation iast, Data data) throws Anomaly;
    Result visit(IASTinteger iast, Data data) throws Anomaly;
    Result visit(IASTinvocation iast, Data data) throws Anomaly;
    Result visit(IASTlambda iast, Data data) throws Anomaly;
    Result visit(IASTloop iast, Data data) throws Anomaly;
    Result visit(IASToperator iast, Data data) throws Anomaly;
    Result visit(IASTfieldRead iast, Data data) throws Anomaly;
    Result visit(IASTself iast, Data data) throws Anomaly;
    Result visit(IASTsend iast, Data data) throws Anomaly;
    Result visit(IASTsequence iast, Data data) throws Anomaly;
    Result visit(IASTstring iast, Data data) throws Anomaly;
    Result visit(IASTsuper iast, Data data) throws Anomaly;
    Result visit(IASTtry iast, Data data) throws Anomaly;
    Result visit(IASTunaryOperation iast, Data data) throws Anomaly;
    Result visit(IASTvariable iast, Data data) throws Anomaly;
    Result visit(IASTfieldWrite iast, Data data) throws Anomaly;
}
