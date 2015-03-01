/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import com.paracamplus.ilp9.interfaces.Inamed;
import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IClass;
import com.paracamplus.ilp9.interpreter.interfaces.IClassEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IInstance;
import com.paracamplus.ilp9.interpreter.interfaces.IMethod;

public abstract class ILP9AbstractClass implements IClass, Inamed {

    public ILP9AbstractClass (IClassEnvironment classEnvironment,
                              String className, 
                              String superClassName,
                              String[] fieldNames,
                              IMethod[] methods ) 
        throws EvaluationException {
        this.className = className;
        if ( "Object".equals(className)) {
            this.superClass = this; // HACK!
        } else {
            this.superClass = classEnvironment.getILP9Class(superClassName);
        }
        this.properFieldNames = fieldNames;
        this.inheritedFieldCount = superClass.getTotalFieldCount();
        this.totalFieldCount = inheritedFieldCount + properFieldNames.length; 
        this.methodDictionary = new HashMap<>();
        Map<String, IMethod> superDictionary = superClass.getMethodDictionary();
        for ( IMethod method : superDictionary.values() ) {
            methodDictionary.put(method.getName(), method);
        }
        for ( IMethod method : methods ) {
            methodDictionary.put(method.getName(), method);
            method.setDefiningClass(this);
        }
        classEnvironment.addILP9Class(this);
    }
    private final String className;
    private final IClass superClass;
    private final String[] properFieldNames;
    private final int totalFieldCount;
    private final int inheritedFieldCount;
    private final Map<String, IMethod> methodDictionary;

    public String getName() {
        return className;
    }

    public IClass getSuperClass() throws EvaluationException {
        return superClass;
    }
    
    public int getTotalFieldCount () {
        return totalFieldCount;
    }
    
    public String[] getTotalFieldNames() {
        String[] fieldNames = new String[getTotalFieldCount()];
        String[] inheritedFieldNames = superClass.getTotalFieldNames();
        for ( int i=0 ; i<inheritedFieldCount ; i++ ) {
            fieldNames[i] = inheritedFieldNames[i];
        }
        for ( int i=inheritedFieldCount ; i<getTotalFieldCount() ; i++ ) {
            fieldNames[i] = properFieldNames[i-inheritedFieldCount];
        }
        return fieldNames;
    }
    
    public String[] getProperFieldNames () {
        return properFieldNames;
    }
    
    public int getOffset(String fieldName) throws EvaluationException {
        String[] properFieldNames = getProperFieldNames();
        for ( int i=0 ; i<properFieldNames.length ; i++) {
            String properFieldName = properFieldNames[i];
            if ( properFieldName.equals(fieldName) ) {
                return getSuperClass().getTotalFieldCount() + i; 
            }
        }
        return getSuperClass().getOffset(fieldName);
    }
    
    public Map<String, IMethod> getMethodDictionary () {
        return methodDictionary;
    }

    public Object send(Interpreter interpreter, 
                       IInstance receiver, 
                       String message,
                       Object[] arguments) throws EvaluationException {
        IMethod method = getMethodDictionary().get(message);
        if ( method == null ) {
            String msg = "Does not understand " + message;
            throw new EvaluationException(msg);
        }
        if ( arguments.length != method.getMethodArity() ) {
            String msg = "Wrong arity for " + message;
            throw new EvaluationException(msg);
        }
        List<Object> functionArguments = new Vector<>();
        functionArguments.add(receiver);
        for ( Object argument : arguments ) {
            functionArguments.add(argument);
        }
        return method.apply(interpreter, functionArguments.toArray()); 
    }
}
