package com.paracamplus.ilp9.interfaces;

import com.paracamplus.ilp9.annotation.OrNull;

public interface IASTclassDefinition extends IASTdeclaration, Inamed {
	@OrNull String getSuperClassName();
	String[] getProperFieldNames();
	int getProperFieldCount();
	String[] getProperMethodNames();
	int getProperMethodCount();
	IASTmethodDefinition[] getProperMethodDefinitions();
}
