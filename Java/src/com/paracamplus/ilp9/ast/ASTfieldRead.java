/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfieldRead;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTfieldRead extends ASTexpression
implements IASTfieldRead {

    public ASTfieldRead (String fieldName, IASTexpression target) {
        this.fieldName = fieldName;
        this.target = target;
    }
    private final String fieldName;
    private final IASTexpression target;
    
    public IASTexpression getTarget() {
        return target;
    }

    public String getFieldName() {
        return fieldName;
    }

    public <Result, Data, Anomaly extends Throwable> Result 
        accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
                throws Anomaly {
        return visitor.visit(this, data);
    }
}
