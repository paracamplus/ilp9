package com.paracamplus.ilp9.interfaces;

public abstract interface IASTconstant extends IASTexpression {
	/** The accurate description of the constant */
    String getDescription();
    /** The constant as an accurate Java value */
    abstract Object getValue ();
}
