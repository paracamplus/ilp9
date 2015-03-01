/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.ast.ASTlambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.Inamed;

public class ASTClambda extends ASTlambda 
implements IASTClambda, Inamed {
    
    public ASTClambda (String closureName,
                       IASTvariable[] variables, 
                       IASTexpression body) {
        super(variables, body);
        this.closedVariables = new HashSet<>();
        this.closureName = closureName;
    }
    private final Set<IASTvariable> closedVariables;
    private final String closureName;

    public Set<IASTvariable> getClosedVariables() {
        return closedVariables;
    }

    public void setClosedVariables(Set<IASTClocalVariable> closedVariables) {
        this.closedVariables.addAll(closedVariables);
    }

    public String getName() {
        return closureName;
    }
    
    public <Result, Data, Anomaly extends Throwable> Result 
    accept(IASTCvisitor<Result, Data, Anomaly> visitor, Data data)
        throws Anomaly {
    return visitor.visit(this, data);
}
}
