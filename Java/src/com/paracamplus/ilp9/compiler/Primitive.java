/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.compiler.interfaces.IPrimitive;


public class Primitive implements IPrimitive {

    public Primitive(String name, String cName, int arity) {
        this.name = name;
        this.cName = cName;
        this.arity = arity;
    }
    private final String name;
    private final String cName;
    private final int arity;

    public String getName() {
        return name;
    }

    public String getCName() {
        return cName;
    }
    
    public int getArity () {
        return arity;
    }
}
