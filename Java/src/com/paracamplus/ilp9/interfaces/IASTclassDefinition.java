package com.paracamplus.ilp9.interfaces;


public interface IASTclassDefinition extends IASTdeclaration, Inamed {
	String getSuperClassName();
	String[] getProperFieldNames();
	String[] getProperMethodNames();
	IASTmethodDefinition[] getProperMethodDefinitions();
}
