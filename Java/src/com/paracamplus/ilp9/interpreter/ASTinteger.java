package com.paracamplus.ilp9.interpreter;

import java.math.BigInteger;

import com.paracamplus.ilp9.interfaces.IASTinteger;

public class ASTinteger extends ASTconstant implements IASTinteger {
    
    public ASTinteger (String description) {
        super(description, new BigInteger(description));
    }
    public BigInteger getValue() {
        return (BigInteger) super.getValue();
    }
}
