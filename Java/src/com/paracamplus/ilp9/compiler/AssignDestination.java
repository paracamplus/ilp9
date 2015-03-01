/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.compiler.interfaces.IDestination;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class AssignDestination implements IDestination {

    // FIXME
    
    public AssignDestination (IASTvariable variable) {
        this.variable = variable;
    }
    private final IASTvariable variable;
    
    public String compile() {
        return variable.getMangledName() + " = ";
    }

}
