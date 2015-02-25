package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;

public class ASTclassDefinition extends ASTnamed
implements IASTclassDefinition {
    
    public ASTclassDefinition (String className,
                               String superClassName,
                               String[] fieldNames,
                               IASTmethodDefinition[] methods) {
        super(className);
        this.superClassName = superClassName;
        this.fieldNames = fieldNames;
        this.methods = methods;
        this.methodNames = new String[methods.length];
        for ( int i=0 ; i<methods.length ; i++ ) {
            this.methodNames[i] = 
                    this.methods[i].getFunctionVariable().getName();
        }
    }
    private final String superClassName;
    private final String[] fieldNames;
    private final IASTmethodDefinition[] methods;
    private final String[] methodNames;

    public String getSuperClassName() {
        return superClassName;
    }

    public String[] getProperFieldNames() {
        return fieldNames;
    }

    public int getProperFieldCount() {
        return fieldNames.length;
    }

    public String[] getProperMethodNames() {
        return methodNames;
    }

    public int getProperMethodCount() {
        return methodNames.length;
    }

    public IASTmethodDefinition[] getProperMethodDefinitions() {
        return methods;
    }
}
