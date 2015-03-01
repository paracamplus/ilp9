/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.primitive;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;

public abstract class UnaryPrimitive extends Primitive {
    
    public UnaryPrimitive(String name) {
        super(name);
    }

    public int getArity () {
        return 1;
    }
    
    public abstract Object apply(Object arg1) throws EvaluationException; 
}
