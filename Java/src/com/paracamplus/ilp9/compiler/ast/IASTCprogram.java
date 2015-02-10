package com.paracamplus.ilp9.compiler.ast;

import java.util.List;

import com.paracamplus.ilp9.interfaces.IASTprogram;

public interface IASTCprogram extends IASTprogram {
    public List<IASTGlobalVariable> getGlobalVariables();
    public void setGlobalVariables(List<IASTGlobalVariable> gvs);
}
