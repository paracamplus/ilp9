/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTloop extends ASTinstruction implements IASTloop {
    
    public ASTloop (IASTexpression condition, IASTexpression body) {
        this.condition = condition;
        this.body = body;
    }
    private final IASTexpression condition;
    private final IASTexpression body;

    public IASTexpression getCondition() {
        return condition;
    }

    public IASTexpression getBody() {
        return body;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
