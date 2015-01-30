package com.paracamplus.ilp9.interfaces;

public interface IASTsend extends IASTexpression {
	 String getMethodName ();
	 IASTexpression getReceiver ();
	 IASTexpression[] getArguments ();
}
