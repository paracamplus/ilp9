/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.Inamed;
import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IClass;
import com.paracamplus.ilp9.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IMethod;
import com.paracamplus.ilp9.interpreter.interfaces.ISuperCallInformation;

public class ILP9Method extends Function
implements IMethod, Inamed {
    
    public ILP9Method (String methodName,
                       String definingClassName,
                       IASTvariable[] variables, 
                       IASTexpression body ) {
        super(variables, body, new EmptyLexicalEnvironment());
        this.methodName = methodName;
        this.definingClassName = definingClassName;
    }
    private final String methodName;
    private final String definingClassName;
    private IClass definingClass;

    public String getName() {
        return methodName;
    }

    public String getDefiningClassName() {
        return definingClassName;
    }
    
    public IClass getDefiningClass() {
        return definingClass;
    }
    public void setDefiningClass(IClass clazz) {
        definingClass = clazz;
    }
    
    public int getMethodArity() {
        return getArity() - 1;
    }
    
    @Override
    public Object apply(Interpreter interpreter, Object[] arguments) 
            throws EvaluationException {
        if ( arguments.length != getArity() ) {
            String msg = "Wrong arity";
            throw new EvaluationException(msg);
        }
        
        ILexicalEnvironment lexenv2 = getClosedEnvironment();
        ISuperCallInformation isci = 
                new SuperCallInformation(arguments, this);
        lexenv2 = lexenv2.extend(isci);
        
        IASTvariable[] variables = getVariables();
        for ( int i=0 ; i<arguments.length ; i++ ) {
            lexenv2 = lexenv2.extend(variables[i], arguments[i]);
        }
        return getBody().accept(interpreter, lexenv2);
    }
}
