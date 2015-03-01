/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTlambda extends ASTexpression implements IASTlambda {
    
    public ASTlambda (IASTvariable[] variables, IASTexpression body) {
        this.variables = variables;
        this.body = body;
    }
    private final IASTvariable[] variables;
    private final IASTexpression body;

    public IASTvariable[] getVariables() {
        return variables;
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
