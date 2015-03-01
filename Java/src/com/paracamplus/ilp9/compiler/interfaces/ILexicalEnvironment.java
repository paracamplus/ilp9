/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.interfaces;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IEnvironment;

public interface ILexicalEnvironment 
extends IEnvironment<IASTvariable, Object, CompilationException> {  

    // Void FIXME ???
    
    // A touch of covariance:
    ILexicalEnvironment extend(IASTvariable variable, Object value);
    ILexicalEnvironment getNext() throws CompilationException;
}
