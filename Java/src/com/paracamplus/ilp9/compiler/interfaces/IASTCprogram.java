package com.paracamplus.ilp9.compiler.interfaces;

import java.util.List;
import java.util.Set;

import com.paracamplus.ilp9.interfaces.IASTprogram;

public interface IASTCprogram extends IASTprogram {
    Set<IASTCGlobalVariable> getGlobalVariables();
    void setGlobalVariables(Set<IASTCGlobalVariable> gvs);
    void addClosureDefinition(IASTClambda f);
    List<IASTClambda> getClosureDefinitions ();
}
