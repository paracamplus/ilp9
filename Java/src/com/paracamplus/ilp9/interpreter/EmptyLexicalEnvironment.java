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

public class EmptyLexicalEnvironment implements ILexicalEnvironment {
    
    public EmptyLexicalEnvironment() {}

    public boolean isPresent(IASTvariable key) {
        return false;
    }

    public IASTvariable getKey() throws EvaluationException {
        throw new EvaluationException("Really empty environment");
    }

    public Object getValue(IASTvariable key) throws EvaluationException {
        throw new EvaluationException("No such variable " + key.getName());
    }

    public void update(IASTvariable key, Object value)
            throws EvaluationException {
        throw new EvaluationException("Empty environment");
    }

    public ISuperCallInformation getSuperCallInformation()
            throws EvaluationException {
        String msg = "No such information";
        throw new EvaluationException(msg);
    }

    public boolean isEmpty() {
        return true;
    }

    public ILexicalEnvironment extend(IASTvariable variable, Object value) {
        return new LexicalEnvironment(variable, value, this);
    }

    public ILexicalEnvironment extend(ISuperCallInformation isci) {
        // Dependence on SuperCallInformationLexicalEnvironment
        return new SuperCallInformationLexicalEnvironment(isci, this);
    }

    public ILexicalEnvironment getNext() throws EvaluationException {
        throw new EvaluationException("Completely empty environment");
    }
}
