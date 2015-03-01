/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.primitive;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;

public class Throw extends UnaryPrimitive {

    public Throw () {
        super("throw");
    }
    
    @SuppressWarnings("serial")
    public static class ThrownException extends EvaluationException {
        public ThrownException (Object value) {
            super("Throwing value");
            this.value = value;
        }
        private final Object value;
        
        public Object getThrownValue () {
            return value;
        }
    }
    
    public Object apply (Object value) throws ThrownException {
        ThrownException exc = new ThrownException(value);
        throw exc;
    }
}
