package com.paracamplus.ilp9.compiler.interfaces;

import java.util.List;
import java.util.Set;

import com.paracamplus.ilp9.interfaces.IASTprogram;

public interface IASTCprogram extends IASTprogram {
    Set<IASTCglobalVariable> getGlobalVariables();
    void setGlobalVariables(Set<IASTCglobalVariable> gvs);
    void addClosureDefinition(IASTClambda f);
    List<IASTClambda> getClosureDefinitions ();
}
