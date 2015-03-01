/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCcodefinitions extends ASTcodefinitions
implements IASTCcodefinitions {

    public ASTCcodefinitions (IASTCnamedLambda[] functions, 
                              IASTexpression body ) {
        super(functions, body);
    }

    @Override
    public IASTCnamedLambda[] getFunctions() {
        return (IASTCnamedLambda[]) super.getFunctions();
    }

    public <Result, Data, Anomaly extends Throwable> Result 
    accept(IASTCvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
