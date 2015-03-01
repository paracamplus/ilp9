/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.ISuperCallInformation;

public class SuperCallInformationLexicalEnvironment 
implements ILexicalEnvironment {
    
    public SuperCallInformationLexicalEnvironment (
            ISuperCallInformation isci,
            ILexicalEnvironment next ) {
        this.isci = isci;
        this.next = next;
    }
    private final ISuperCallInformation isci;
    private final ILexicalEnvironment next;
    
    public ISuperCallInformation getSuperCallInformation() {
        return isci;
    }

    public ILexicalEnvironment getNext() {
        return next;
    }

    public boolean isPresent(IASTvariable key) {
        return getNext().isPresent(key);
    }

    public IASTvariable getKey() throws EvaluationException {
        String msg = "No key on " + this.getClass().getName();
        throw new EvaluationException(msg);
    }

    public Object getValue(IASTvariable key) throws EvaluationException {
        String msg = "No value on " + this.getClass().getName();
        throw new EvaluationException(msg);
    }

    public void update(IASTvariable key, Object value)
            throws EvaluationException {
        String msg = "Cannot update " + this.getClass().getName();
        throw new EvaluationException(msg);
    }

    public boolean isEmpty() {
        return getNext().isEmpty();
    }

    public ILexicalEnvironment extend(ISuperCallInformation isci) {
        return new SuperCallInformationLexicalEnvironment(isci, this);
    }

    public ILexicalEnvironment extend(IASTvariable variable, Object value) {
        return new LexicalEnvironment(variable, value, this);
    }
}
