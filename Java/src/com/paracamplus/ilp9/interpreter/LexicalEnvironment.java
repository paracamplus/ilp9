package com.paracamplus.ilp9.interpreter;

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
        return value;
    }
    public void updateValue (Object value) {
        this.value = value;
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

    public void update(IASTvariable key, Object value) throws EvaluationException {
        if ( key.getName().equals(getKey().getName()) ) {
            updateValue(value);
        } else {
            getNext().update(key, value);
        }
    }

    public Object getValue(IASTvariable key) throws EvaluationException {
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

        public IASTvariable getKey() throws EvaluationException {
            throw new EvaluationException("Really empty environment");
        }

        public Object getValue(IASTvariable key) throws EvaluationException {
            throw new EvaluationException("No such variable " + key.getName());
        }

        public void update(IASTvariable key, Object value) throws EvaluationException {
            throw new EvaluationException("Empty environment");
        }

        public boolean isEmpty() {
            return true;
        }

        public ILexicalEnvironment extend(IASTvariable variable, Object value) {
            return new LexicalEnvironment(variable, value, this);
        }

        public ILexicalEnvironment getNext() throws EvaluationException {
            throw new EvaluationException("Completely empty environment");
        }
    }
    public static final ILexicalEnvironment EMPTY = 
            new EmptyLexicalEnvironment();
    
}
