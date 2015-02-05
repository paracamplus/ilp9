package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class Function implements IFunction {
    
    public Function (IASTvariable[] variables, 
                     IASTexpression body, 
                     ILexicalEnvironment lexenv) {
        this.variables = variables;
        this.body = body;
        this.lexenv = lexenv;
    }
    private final IASTvariable[] variables;
    private final IASTexpression body;
    private final ILexicalEnvironment lexenv;
    // hint possible name ???

    public int getArity() {
        return variables.length;
    }

    public Object apply(Interpreter interpreter, Object[] argument) 
            throws EvaluationException {
        if ( argument.length != getArity() ) {
            String msg = "Wrong arity";
            throw new EvaluationException(msg);
        }
        
        ILexicalEnvironment lexenv2 = lexenv;
        for ( int i=0 ; i<argument.length ; i++ ) {
            lexenv2 = lexenv2.extend(variables[i], argument[i]);
        }
        return body.accept(interpreter, lexenv2);
    }
}
