/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.interfaces;

import com.paracamplus.ilp9.interpreter.Interpreter;

public interface IInstance {
    IClass classOf();
    Object read (String fieldName) throws EvaluationException;
    Object write (String fieldName, Object value) throws EvaluationException;
    Object send (Interpreter interpreter, 
                 String message, 
                 Object[] arguments) throws EvaluationException;
}
