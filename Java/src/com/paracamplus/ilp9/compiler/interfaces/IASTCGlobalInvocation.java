package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTinvocation;

public interface IASTCGlobalInvocation extends IASTinvocation {
    IASTCGlobalVariable getFunction();
}
