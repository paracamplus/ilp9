/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;

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
    
    public <Result, Data, Anomaly extends Throwable> Result 
    accept(IASTCvisitor<Result, Data, Anomaly> visitor, Data data)
        throws Anomaly {
    return visitor.visit(this, data);
}
}
