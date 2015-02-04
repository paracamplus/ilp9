package com.paracamplus.ilp9.interpreter.operator;

import com.paracamplus.ilp9.interpreter.BinaryOperator;
import com.paracamplus.ilp9.interpreter.EvaluationException;

public class And extends BinaryOperator {
    
    public And () {
        super("&");
    }
    
    public Object apply (Object arg1, Object arg2) 
            throws EvaluationException {
        if ( arg1 instanceof Boolean ) {
            Boolean b1 = (Boolean) arg1;
            if ( ! b1.booleanValue() ) {
              return Boolean.FALSE;
            }
        }
        // Here, arg1 cannot be false!
        if ( arg2 instanceof Boolean ) {
          Boolean b2 = (Boolean) arg2;
          if ( ! b2.booleanValue() ) {
            return Boolean.FALSE;
          }
        }
        // and here, arg2 cannot be false!
        return arg2;
    }
}

// end of And.java
