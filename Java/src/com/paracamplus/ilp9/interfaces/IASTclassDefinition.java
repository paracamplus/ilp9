package com.paracamplus.ilp9.interfaces;

import java.util.Arrays;


public interface IASTclassDefinition extends IASTdeclaration, Inamed {
	String getSuperClassName();
	
	String[] getProperFieldNames();
	
    default String[] getProperMethodNames() {
        return Arrays.stream(getProperMethodDefinitions()).map(
                (md) -> md.getMethodName() )
                .toArray(String[]::new);
    }
    
	IASTmethodDefinition[] getProperMethodDefinitions();
}
