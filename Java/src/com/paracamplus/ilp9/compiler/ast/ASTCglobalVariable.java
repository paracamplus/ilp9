package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;

public class ASTCglobalVariable extends ASTCvariable 
implements IASTCglobalVariable {

    public ASTCglobalVariable (String name) {
        super(name);
    }
    
    @Override
    public int hashCode() {
        return getName().hashCode();
    }
 
    @Override
    public boolean equals(Object obj) {
        if ( this == obj ) {
            return true;
        }
        if (obj == null ) {
            return false;
        }
        if ( obj.getClass() == ASTCglobalFunctionVariable.class ) {
            return ((ASTCglobalFunctionVariable) obj).getName()
                    .equals(getName());
        }
        return false;
    }
}
