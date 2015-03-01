/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTbinaryOperation extends ASTexpression implements IASTbinaryOperation {

    public ASTbinaryOperation (IASToperator operator,
                               IASTexpression leftOperand,
                               IASTexpression rightOperand ) {
        this.operator = operator;
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
    }
    private final IASToperator operator;
    private final IASTexpression leftOperand;
    private final IASTexpression rightOperand;
    
    public IASToperator getOperator() {
        return operator;
    }

    public IASTexpression[] getOperands() {
        return new IASTexpression[]{ leftOperand, rightOperand };
    }

    public IASTexpression getLeftOperand() {
        return leftOperand;
    }

    public IASTexpression getRightOperand() {
        return rightOperand;
    }
    
    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
