/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;

public class ASTClocalFunctionVariable extends ASTClocalVariable
implements IASTClocalFunctionVariable {

    public ASTClocalFunctionVariable (String name) {
        super(name);
    }
}
