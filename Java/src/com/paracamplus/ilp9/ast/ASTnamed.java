/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.Inamed;

public abstract class ASTnamed extends AST implements Inamed {
    
    public ASTnamed (String name) {
        this.name = name;
    }
    private String name;
    
    public String getName() {
        return name;
    }
}
