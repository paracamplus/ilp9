package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.interfaces.IASTinvocation;

public interface IASTGlobalInvocation extends IASTinvocation {
    IASTGlobalVariable getFunction();
}
