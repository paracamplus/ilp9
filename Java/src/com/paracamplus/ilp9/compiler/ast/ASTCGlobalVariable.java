package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;

public class ASTCGlobalVariable extends ASTCvariable 
implements IASTCGlobalVariable {

    public ASTCGlobalVariable (String name) {
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
        if ( obj.getClass() == ASTCGlobalFunctionVariable.class ) {
            return ((ASTCGlobalFunctionVariable) obj).getName()
                    .equals(getName());
        }
        return false;
    }
}
