/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTmethodDefinition extends ASTfunctionDefinition
implements IASTmethodDefinition {
    
    public ASTmethodDefinition(IASTvariable methodVariable, 
                               IASTvariable[] variables,
                               IASTexpression body,
                               String methodName,
                               String definingClassName ) {
        super(methodVariable, variables, body);
        this.definingClassName = definingClassName;
        this.methodName = methodName;
    }
    private final String definingClassName;
    private final String methodName;

    public String getDefiningClassName() {
        return definingClassName;
    }
    
    public String getMethodName() {
        return methodName;
    }
}
