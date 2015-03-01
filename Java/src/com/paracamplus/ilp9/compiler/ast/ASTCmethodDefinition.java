/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.annotation.OrNull;
import com.paracamplus.ilp9.ast.ASTmethodDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTCmethodDefinition extends ASTmethodDefinition 
implements IASTCmethodDefinition {
    public ASTCmethodDefinition (
            IASTvariable methodVariable,
            IASTvariable[] variables, 
            IASTexpression body,
            String methodName,
            IASTCclassDefinition definingClass ) {
        super(methodVariable, variables, body, 
                methodName, definingClass.getName());
        this.definingClass = definingClass;
        this.closedVariables = new HashSet<>();
    }
    private final IASTCclassDefinition definingClass;
    private final Set<IASTvariable> closedVariables;
    
    public IASTCclassDefinition getDefiningClass () {
        return definingClass;
    }

    public String getCName() {
        return "ilp__" + getFunctionVariable().getMangledName();
    }

    public Set<IASTvariable> getClosedVariables() {
        return closedVariables;
    }

    public void setClosedVariables(Set<IASTClocalVariable> newvars) {
        this.closedVariables.addAll(closedVariables);
    }

    public @OrNull IASTCmethodDefinition findSuperMethod() {
        IASTCclassDefinition superClass = getDefiningClass().getSuperClass();
        IASTCmethodDefinition[] methods = superClass.getTotalMethodDefinitions();
        for ( IASTCmethodDefinition method : methods ) {
            if ( getMethodName().equals(method.getMethodName()) ) {
                return method;
            }
        }
        return null;
    }
}
