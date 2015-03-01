/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IClassEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IMethod;

public class ILP9Class extends ILP9AbstractClass {
    
    public ILP9Class (IClassEnvironment classEnvironment,
                      String className, 
                      String superClassName,
                      String[] fieldNames,
                      IMethod[] methods ) throws EvaluationException {
        super(classEnvironment, className, superClassName, fieldNames, methods);
    }
}
