/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTtry extends ASTinstruction implements IASTtry {
    
    public ASTtry (IASTexpression body,
                   IASTlambda catcher,
                   IASTexpression finallyer ) {
        this.body = body;
        this.catcher = catcher;
        this.finallyer = finallyer;
    }
    private final IASTexpression body;
    private final IASTlambda catcher;
    private final IASTexpression finallyer;

    public IASTexpression getBody() {
        return body;
    }

    public IASTlambda getCatcher() {
        return catcher;
    }

    public IASTexpression getFinallyer() {
        return finallyer;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
