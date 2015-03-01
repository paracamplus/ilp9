/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTunaryOperation extends ASTexpression implements IASTunaryOperation {
    
    public ASTunaryOperation (IASToperator operator, IASTexpression operand) {
        this.operator = operator;
        this.operand = operand;
    }
    private final IASToperator operator;
    private final IASTexpression operand;
    
    public IASTexpression getOperand() {
        return operand;
    }

    public IASToperator getOperator() {
        return operator;
    }

    public IASTexpression[] getOperands() {
        return new IASTexpression[]{ getOperand() };
    }
    
    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
