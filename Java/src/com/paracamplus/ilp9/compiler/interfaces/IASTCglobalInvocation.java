package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTinvocation;

public interface IASTCglobalInvocation extends IASTinvocation {
    IASTCglobalVariable getFunction();
}
