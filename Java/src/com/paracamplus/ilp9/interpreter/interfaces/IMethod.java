/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.interfaces;

public interface IMethod extends Invocable {
    String getName();
    IClass getDefiningClass();
    void setDefiningClass(IClass clazz);
    int getMethodArity();
}
