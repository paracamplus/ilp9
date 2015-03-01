/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTClocalFunctionInvocation extends ASTinvocation
implements IASTClocalFunctionInvocation {

    public ASTClocalFunctionInvocation (IASTexpression function, 
                                       IASTexpression[] arguments) {
        super(function, arguments);
    }
}
