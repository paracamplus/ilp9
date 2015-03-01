/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import com.paracamplus.ilp9.ast.ASTinvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcomputedInvocation;
import com.paracamplus.ilp9.interfaces.IASTexpression;

public class ASTCcomputedInvocation  extends ASTinvocation
implements IASTCcomputedInvocation {
    
    public ASTCcomputedInvocation (IASTexpression function, 
                                  IASTexpression[] arguments) {
        super(function, arguments);
    }
}
