/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IClass;
import com.paracamplus.ilp9.interpreter.interfaces.IInstance;

public class ILP9Instance implements IInstance {
    
    public ILP9Instance (IClass clazz, Object[] fields) 
            throws EvaluationException {
        this.clazz = clazz;
        this.fields = fields;
        if ( fields.length != clazz.getTotalFieldCount() ) {
            String msg = "Wrong number of initial values for " 
                    + clazz.getName();
            throw new EvaluationException(msg);
        }
    }
    private final IClass clazz;
    private final Object[] fields;
    
    public IClass classOf() {
        return clazz;
    }

    public Object read(String fieldName) 
            throws EvaluationException {
        int offset = classOf().getOffset(fieldName);
        return fields[offset];
    }

    public Object write(String fieldName, Object value) 
            throws EvaluationException {
        int offset = classOf().getOffset(fieldName);
        Object old = fields[offset];
        fields[offset] = value;
        return old;
    }

    public Object send(Interpreter interpreter, 
                       String message, 
                       Object[] arguments ) throws EvaluationException {
        return classOf().send(interpreter, this, message, arguments);
    }
    
    // Useful for debug in Eclipse
    public String toString () {
        StringBuffer sb = new StringBuffer();
        sb.append("<");
        sb.append(clazz.getName());
        for ( String fieldName : clazz.getTotalFieldNames() ) {
            Object value = "?";
            try {
                value = read(fieldName);
            } catch (EvaluationException e) {
                // ignore
            }
            sb.append(":");
            sb.append(fieldName);
            sb.append("=");
            sb.append(value);
        }
        sb.append(">");
        return sb.toString();
    }
}
