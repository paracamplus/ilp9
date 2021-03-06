/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.interfaces;

@SuppressWarnings("serial")
public class EvaluationException extends Exception {

    public EvaluationException(String msg) {
        super(msg);
    }

    public EvaluationException(Exception e) {
        super(e);
    }
}
