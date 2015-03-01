/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASTcodefinitions;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTcodefinitions extends ASTexpression 
implements IASTcodefinitions {

    public ASTcodefinitions (IASTnamedLambda[] functions, 
                             IASTexpression body ) {
        this.functions = functions;
        this.body = body;
    }
    private final IASTnamedLambda[] functions;
    private final IASTexpression body;
    
    public IASTnamedLambda[] getFunctions() {
        return functions;
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
