/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter;

import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IClass;
import com.paracamplus.ilp9.interpreter.interfaces.IClassEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IMethod;
import com.paracamplus.ilp9.interpreter.interfaces.IPrimitive;
import com.paracamplus.ilp9.interpreter.primitive.Newline;
import com.paracamplus.ilp9.interpreter.primitive.Print;

public class ClassEnvironment implements IClassEnvironment {
    
    public ClassEnvironment (Writer out) throws EvaluationException {
        this.clazzes = new HashMap<>();
        IClass objectClass = new ILP9ObjectClass(this);
        initializeClassEnvironment(objectClass, out);
    }
    private final Map<String, IClass> clazzes;

    public IClass getILP9Class(String name) throws EvaluationException {
        IClass clazz = clazzes.get(name);
        if ( clazz == null ) {
            String msg = "No such class " + name;
            throw new EvaluationException(msg);
        }
        return clazz;
    }

    public void addILP9Class(IClass clazz) {
        clazzes.put(clazz.getName(), clazz);
    }
    
    //
    
    protected void initializeClassEnvironment(IClass clazz, Writer out) {
        addPrimitiveAsMethod(clazz, new Print(out));
        addPrimitiveAsMethod(clazz, new Newline(out));
    }

    protected void addPrimitiveAsMethod (final IClass clazz, final IPrimitive primitive) {
        IMethod method = new IMethod() {
            public String getName() {
                return primitive.getName();
            }
            public IClass getDefiningClass() {
                return clazz;
            }
            public void setDefiningClass (IClass clazz) {
                String msg = "Hard coded immutable class";
                throw new RuntimeException(msg);
            }
            public int getMethodArity() {
                return primitive.getArity() - 1;
            }
            public int getArity() {
                return primitive.getArity();
            }
            public Object apply (Interpreter interpreter, Object[] arguments) 
            throws EvaluationException {
                return primitive.apply(interpreter, arguments);
            }
        };
        clazz.getMethodDictionary().put(primitive.getName(), method);
    }
}
