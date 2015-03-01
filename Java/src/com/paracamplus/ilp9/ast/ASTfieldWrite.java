/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvisitor;
import com.paracamplus.ilp9.interfaces.IASTfieldWrite;

public class ASTfieldWrite extends ASTexpression
implements IASTfieldWrite {

    public ASTfieldWrite (String fieldName,
                          IASTexpression target,
                          IASTexpression value) {
        this.fieldName = fieldName;
        this.target = target;
        this.value = value;
    }
    private final String fieldName;
    private final IASTexpression target;
    private final IASTexpression value;
    
    public IASTexpression getTarget() {
        return target;
    }

    public String getFieldName() {
        return fieldName;
    }
    
    public IASTexpression getValue() {
        return value;
    }

    public <Result, Data, Anomaly extends Throwable> Result 
        accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
                throws Anomaly {
        return visitor.visit(this, data);
    }
}
