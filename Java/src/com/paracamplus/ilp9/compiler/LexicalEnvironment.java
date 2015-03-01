/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler;

import com.paracamplus.ilp9.compiler.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class LexicalEnvironment implements ILexicalEnvironment {

    public LexicalEnvironment (IASTvariable variable, 
                               Object value,
                               ILexicalEnvironment next ) {
        this.variable = variable;
        this.value = value;
        this.next = next;
    }
    private final IASTvariable variable;
    private Object value;
    private final ILexicalEnvironment next;
    
    public IASTvariable getKey() {
        return variable;
    }
    public Object getValue() {
        return value; // useless
    }
    public void updateValue (Object value) {
        this.value = value; // useless
    }
    
    public boolean isPresent(IASTvariable key) {
        if ( key.getName().equals(getKey().getName()) ) {
            return true;
        } else {
            return getNext().isPresent(key);
        }
    }

    public ILexicalEnvironment extend(IASTvariable variable, Object value) {
        return new LexicalEnvironment(variable, value, this);
    }

    public void update(IASTvariable key, Object value) 
            throws CompilationException {
        if ( key.getName().equals(getKey().getName()) ) {
            updateValue(value);
        } else {
            getNext().update(key, value);
        }
    }

    public Object getValue(IASTvariable key) throws CompilationException {
        if ( key.getName().equals(getKey().getName()) ) {
            return getValue();
        } else {
            return getNext().getValue(key);
        }
    }

    public boolean isEmpty() {
        return false;
    }
 
    public ILexicalEnvironment getNext() {
        return next;
    }

    // The empty lexical environment:
    
    public static class EmptyLexicalEnvironment 
    implements ILexicalEnvironment {

        public boolean isPresent(IASTvariable key) {
            return false;
        }

        public IASTvariable getKey() throws CompilationException {
            throw new CompilationException("Really empty environment");
        }

        public Object getValue(IASTvariable key) throws CompilationException {
            throw new CompilationException("No such variable " + key.getName());
        }

        public void update(IASTvariable key, Object value) throws CompilationException {
            throw new CompilationException("Empty environment");
        }

        public boolean isEmpty() {
            return true;
        }

        public ILexicalEnvironment extend(IASTvariable variable, Object value) {
            return new LexicalEnvironment(variable, value, this);
        }

        public ILexicalEnvironment getNext() throws CompilationException {
            throw new CompilationException("Completely empty environment");
        }
    }
    public static final ILexicalEnvironment EMPTY = 
            new EmptyLexicalEnvironment();

}
