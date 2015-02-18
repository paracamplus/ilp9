package com.paracamplus.ilp9.interpreter.operator;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;

public class Xor extends BinaryOperator {
    
    public Xor () {
        super("^");
    }
    
    public Object apply (Object arg1, Object arg2) 
            throws EvaluationException {
        if ( arg1 instanceof Boolean ) {
            Boolean b1 = (Boolean) arg1;
            if ( b1.booleanValue() ) {
                // Here arg1 is the true boolean:
                if ( arg2 instanceof Boolean ) {
                    Boolean b2 = (Boolean) arg2;
                    if ( b2.booleanValue() ) {
                        // Here arg2 is the true boolean:
                        return Boolean.FALSE;
                    } else {
                        // Here arg2 is also the false boolean:
                        return Boolean.TRUE;
                    }
                } else {
                    // Here arg2 is not a boolean hence true
                        return Boolean.FALSE;
                }
            } else {
              // Here arg1 is the boolean false:
              if ( arg2 instanceof Boolean ) {
                Boolean b2 = (Boolean) arg2;
                if ( b2.booleanValue() ) {
                    // Here arg2 is the true boolean:
                    return b2; // True!
                } else {
                    // Here arg2 is also the boolean false:
                  return Boolean.FALSE;
                }
              } else {
                  // here arg2 is not a boolean hence true:
                  return arg2;
              }
            }
        } else {
            // Here arg1 is not a boolean hence true:
            if ( arg2 instanceof Boolean ) {
                Boolean b2 = (Boolean) arg2;
                if ( b2.booleanValue() ) {
                    // Here arg2 is the true boolean:
                    return Boolean.FALSE;
                } else {
                    // Here arg2 is the false boolean:
                    return arg1;
                }
            } else {
                // Here arg2 is not a boolean hence true:
                return Boolean.FALSE;
            }
        }
    }
}

// end of Xor.java
