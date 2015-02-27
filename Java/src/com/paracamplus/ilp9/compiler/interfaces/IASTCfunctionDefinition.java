package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;

public interface IASTCfunctionDefinition 
extends IASTfunctionDefinition, IClosable {
    String getCName();
}
