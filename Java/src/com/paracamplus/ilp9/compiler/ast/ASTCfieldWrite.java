/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTfieldWrite;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldWrite;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTCfieldWrite extends ASTfieldWrite 
implements IASTCfieldWrite {

    public ASTCfieldWrite (IASTCclassDefinition clazz,
                           String fieldName, 
                           IASTexpression target,
                           IASTexpression value ) {
        super(fieldName, target, value);
        this.clazz = clazz;
    }
    private final IASTCclassDefinition clazz;

    public IASTCclassDefinition getDefiningClass () {
        return clazz;
    }
    
    @Override
    public <Result, Data, Anomaly extends Throwable> Result 
    accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
