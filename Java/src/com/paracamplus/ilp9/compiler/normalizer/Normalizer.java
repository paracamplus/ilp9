/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.normalizer;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.ast.ASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCblock;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCmethodDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTcodefinitions;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfieldRead;
import com.paracamplus.ilp9.interfaces.IASTfieldWrite;
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinstantiation;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class Normalizer implements 
 IASTvisitor<IASTexpression, INormalizationEnvironment, CompilationException> {

    public Normalizer (INormalizationFactory factory,
                       IASTCclassDefinition objectClass ) {
        this.factory = factory;
        this.globalVariables = new HashSet<>();
        this.field2classes = new HashMap<>();
        this.classes = new HashMap<>();
        classes.put("Object", objectClass);
    }
    private final INormalizationFactory factory;
    private final Set<IASTvariable> globalVariables;
    private final Map<String, IASTCclassDefinition> classes;
    private final Map<String, IASTCclassDefinition> field2classes;
    
    protected IASTCclassDefinition getClassByName (String className)
            throws CompilationException {
        IASTCclassDefinition cd = classes.get(className);
        if ( cd == null ) {
            String msg = "No class with that name " + className;
            throw new CompilationException(msg);
        }
        return cd;
    }
    
    protected IASTCclassDefinition getClassByFieldName (String fieldName) 
            throws CompilationException {
        IASTCclassDefinition cd = field2classes.get(fieldName);
        if ( cd == null ) {
            String msg = "No class with that field " + fieldName;
            throw new CompilationException(msg);
        }
        return cd;
    }

    public IASTCprogram transform(IASTprogram program) 
            throws CompilationException {
        INormalizationEnvironment env = NormalizationEnvironment.EMPTY;
        IASTclassDefinition[] clazzes = program.getClassDefinitions();
        IASTCclassDefinition[] newclasses = 
                new IASTCclassDefinition[clazzes.length];
        for ( int i=0 ; i<clazzes.length ; i++ ) {
            IASTclassDefinition cd = clazzes[i];
            IASTCclassDefinition newclass = visit(cd, env); 
            newclasses[i] = newclass;
            classes.put(newclass.getName(), newclass);
        }

        IASTfunctionDefinition[] functions = program.getFunctionDefinitions();
        IASTCfunctionDefinition[] funs = 
                new IASTCfunctionDefinition[functions.length];
        for ( IASTfunctionDefinition function : functions ) {
            IASTCglobalFunctionVariable gfv =
                    factory.newGlobalFunctionVariable(function.getName());
            env = env.extend(gfv, gfv);
        }
        for ( int i=0 ; i<functions.length ; i++ ) {
            IASTfunctionDefinition function = functions[i];
            IASTCfunctionDefinition newfunction = visit(function, env);
            funs[i] = newfunction;
        }
        
        IASTexpression body = program.getBody();
        IASTexpression newbody = body.accept(this, env);
        return factory.newProgram(funs, newclasses, newbody);
    }

    public IASTexpression visit(IASTalternative iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTexpression c = iast.getCondition().accept(this, env);
        IASTexpression t = iast.getConsequence().accept(this, env);
        if ( iast.isTernary() ) {
            IASTexpression a = iast.getAlternant().accept(this, env);
            return factory.newAlternative(c, t, a);
        } else {
            IASTexpression whatever = factory.newBooleanConstant("false");
            return factory.newAlternative(c, t, whatever);
        }
    }
    
    public IASTexpression visit(IASTboolean iast, INormalizationEnvironment env)
            throws CompilationException {
        return iast;
    }
    
    public IASTexpression visit(IASTinteger iast, INormalizationEnvironment env)
            throws CompilationException {
        return iast;
    }

    public IASTexpression visit(IASTfloat iast, INormalizationEnvironment env)
            throws CompilationException {
        return iast;
    }

    public IASTexpression visit(IASTstring iast, INormalizationEnvironment env)
            throws CompilationException {
        return iast;
    }

    public IASTexpression visit(IASTsequence iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTexpression[] expressions = iast.getExpressions();
        IASTexpression[] exprs = new IASTexpression[expressions.length];
        for ( int i=0 ; i< expressions.length ; i++ ) {
            exprs[i] = expressions[i].accept(this, env);
        }
        if ( iast.getExpressions().length == 1 ) {
            return exprs[0];
        } else {
            return factory.newSequence(exprs);
        }
    }

    public IASTvariable visit(IASTvariable iast, INormalizationEnvironment env)
            throws CompilationException {
        try {
            return env.renaming(iast);
        } catch (NoSuchLocalVariableException exc) {
            // TODO If we were to know the primitives, we might be more accurate:
            for ( IASTvariable gv : globalVariables ) {
                if ( iast.getName().equals(gv.getName()) ) {
                    return gv;
                }
            }
            IASTvariable gv = factory.newGlobalVariable(iast.getName());
            globalVariables.add(gv);
            return gv;
        }
    }
    
    public IASTexpression visit(IASTassignment iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTvariable variable = iast.getVariable();
        IASTvariable newvariable = visit(variable, env);
        IASTexpression expression = iast.getExpression();
        IASTexpression newexpression = expression.accept(this, env);
        return factory.newAssignment(newvariable, newexpression);
    }

    public IASTexpression visit(IASTunaryOperation iast, INormalizationEnvironment env)
            throws CompilationException {
        IASToperator operator = iast.getOperator();
        IASTexpression operand = iast.getOperand().accept(this, env);
        return factory.newUnaryOperation(operator, operand);
    }

    public IASTexpression visit(IASTbinaryOperation iast, INormalizationEnvironment env)
            throws CompilationException {
        IASToperator operator = iast.getOperator();
        IASTexpression left = iast.getLeftOperand().accept(this, env);
        IASTexpression right = iast.getRightOperand().accept(this, env);
        return factory.newBinaryOperation(operator, left, right);
    }
    
    public IASTexpression visit(IASToperator iast, INormalizationEnvironment env)
            throws CompilationException {
        throw new RuntimeException("Already processed via Operation");
    }

    public IASTexpression visit(IASTblock iast, INormalizationEnvironment env)
            throws CompilationException {
        INormalizationEnvironment newenv = env;
        IASTbinding[] bindings = iast.getBindings();
        IASTCblock.IASTCbinding[] newbindings = 
                new IASTCblock.IASTCbinding[bindings.length];
        for ( int i=0 ; i<bindings.length ; i++ ) {
            IASTbinding binding = bindings[i];
            IASTexpression expr = binding.getInitialisation();
            IASTexpression newexpr = expr.accept(this, env);
            IASTvariable variable = binding.getVariable();
            IASTvariable newvariable = 
                    factory.newLocalVariable(variable.getName());
            newenv = newenv.extend(variable, newvariable);
            newbindings[i] = factory.newBinding(newvariable, newexpr);
        }
        IASTexpression newbody = iast.getBody().accept(this, newenv);
        return factory.newBlock(newbindings, newbody);
    }

    public IASTexpression visit(IASTtry iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTexpression newbody = iast.getBody().accept(this, env);
        IASTlambda newcatcher = null;
        IASTlambda catcher = iast.getCatcher();
        if ( catcher != null ){
            newcatcher = (IASTlambda) catcher.accept(this, env);
        }
        IASTexpression newfinallyer = null;
        IASTexpression finallyer = iast.getFinallyer();
        if ( finallyer != null ) {
            newfinallyer = finallyer.accept(this, env);
        }
        return factory.newTry(newbody, newcatcher, newfinallyer);
    }

    public IASTexpression visit(IASTcodefinitions iast, 
                                INormalizationEnvironment env)
            throws CompilationException {
        IASTnamedLambda[] functions = iast.getFunctions();
        IASTCnamedLambda[] newfunctions = 
                new IASTCnamedLambda[functions.length];
        IASTvariable[] newFunctionVariables = 
                new IASTvariable[functions.length];
        INormalizationEnvironment bodyenv = env;
        for ( int i=0 ; i<functions.length ; i++ ) {
            IASTnamedLambda function = functions[i];
            IASTvariable oldFunctionVar = function.getFunctionVariable();
            IASTvariable newFunctionVar = 
                    factory.newLocalFunctionVariable(oldFunctionVar.getName());
            newFunctionVariables[i] = newFunctionVar;
            bodyenv = bodyenv.extend(oldFunctionVar, newFunctionVar);
        }
        for ( int i=0 ; i<functions.length ; i++ ) {
            IASTnamedLambda function = functions[i];
            IASTvariable newFunctionVar = newFunctionVariables[i]; 
            INormalizationEnvironment newenv = bodyenv;
            IASTvariable[] variables = function.getVariables();
            IASTvariable[] newvariables = new IASTvariable[variables.length];
            for ( int iv=0 ; iv<variables.length ; iv++ ) {
                IASTvariable variable = variables[iv];
                IASTvariable newvariable = 
                        factory.newLocalVariable(variable.getName());
                newvariables[iv] = newvariable;
                newenv = newenv.extend(variable, newvariable);
            }
            IASTexpression newbody = function.getBody().accept(this, newenv);
            newfunctions[i] = factory.newNamedLambda(
                    newFunctionVar, newvariables, newbody);
        }
        IASTexpression newbody = iast.getBody().accept(this, bodyenv);
        return factory.newCodefinitions(newfunctions, newbody);
    }

    public IASTCfunctionDefinition visit(
            IASTfunctionDefinition iast,
            INormalizationEnvironment env) throws CompilationException {
        String functionName = iast.getName();
        IASTvariable[] variables = iast.getVariables();
        IASTvariable[] newvariables = new IASTvariable[variables.length];
        INormalizationEnvironment newenv = env;
        for ( int i=0 ; i<variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            IASTvariable newvariable = 
                    factory.newLocalVariable(variable.getName());
            newvariables[i] = newvariable;
            newenv = newenv.extend(variable, newvariable);
        }
        IASTexpression newbody = iast.getBody().accept(this, newenv);
        IASTvariable functionVariable = 
                factory.newGlobalFunctionVariable(functionName);
        return factory.newFunctionDefinition(
                functionVariable, newvariables, newbody);
    }

    public IASTexpression visit(IASTlambda iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTvariable[] variables = iast.getVariables();
        IASTvariable[] newvariables = new IASTvariable[variables.length];
        INormalizationEnvironment newenv = env;
        for ( int i=0 ; i<variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            IASTvariable newvariable = 
                    factory.newLocalVariable(variable.getName());
            newvariables[i] = newvariable;
            newenv = newenv.extend(variable, newvariable);
        }
        IASTexpression newbody = iast.getBody().accept(this, newenv);
        return factory.newLambda(newvariables, newbody);
    }

    public IASTexpression visit(IASTinvocation iast, 
                                INormalizationEnvironment env)
            throws CompilationException {
        IASTexpression funexpr = iast.getFunction().accept(this, env);
        IASTexpression[] arguments = iast.getArguments();
        IASTexpression[] args = new IASTexpression[arguments.length];
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTexpression argument = arguments[i];
            IASTexpression arg = argument.accept(this, env);
            args[i] = arg;
        }
        if ( funexpr instanceof IASTCglobalVariable ) {
            IASTCglobalVariable f = (IASTCglobalVariable) funexpr;
            return factory.newGlobalInvocation(f, args);
        } else if ( funexpr instanceof IASTClocalFunctionVariable ) {
            IASTClocalFunctionVariable f = (ASTClocalFunctionVariable) funexpr;
            return factory.newLocalFunctionInvocation(f, args);
        } else {
            return factory.newComputedInvocation(funexpr, args);
        }
    }

    public IASTexpression visit(IASTloop iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTexpression newcondition = iast.getCondition().accept(this, env);
        IASTexpression newbody = iast.getBody().accept(this, env);
        return factory.newLoop(newcondition, newbody);
    }
    
    // class related 

    public IASTCclassDefinition visit(IASTclassDefinition iast, 
                                      INormalizationEnvironment env)
            throws CompilationException {
        IASTmethodDefinition[] methods = iast.getProperMethodDefinitions();
        IASTCmethodDefinition[] newmethods =
                new IASTCmethodDefinition[methods.length]; 
        IASTCclassDefinition cd = factory.newClassDefinition(
                iast.getName(),
                getClassByName(iast.getSuperClassName()),
                iast.getProperFieldNames(),
                newmethods );
        for ( String fieldName : iast.getProperFieldNames() ) {
            field2classes.put(fieldName, cd);
        }
        for ( int i=0 ; i<methods.length ; i++ ) {
            IASTmethodDefinition method = methods[i];
            newmethods[i] = visit(method, env, cd);
        }
        return cd;
    }

    public IASTCmethodDefinition visit(
            IASTmethodDefinition iast, 
            INormalizationEnvironment env,
            IASTCclassDefinition definingClass )
            throws CompilationException {
        String methodName = iast.getName();
        IASTvariable[] variables = iast.getVariables();
        IASTvariable[] newvariables = new IASTvariable[variables.length];
        INormalizationEnvironment newenv = env;
        for ( int i=0 ; i<variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            IASTvariable newvariable = 
                    factory.newLocalVariable(variable.getName());
            newvariables[i] = newvariable;
            newenv = newenv.extend(variable, newvariable);
        }
        IASTexpression newbody = iast.getBody().accept(this, newenv);
        IASTvariable methodVariable = 
                factory.newMethodVariable(methodName);
        return factory.newMethodDefinition(
                methodVariable, newvariables, newbody, 
                methodName, definingClass );
    }

    public IASTexpression visit(IASTinstantiation iast, INormalizationEnvironment env)
            throws CompilationException {
        IASTCclassDefinition cd = getClassByName(iast.getClassName());
        List<Object> args = new Vector<Object>();
        for ( IASTexpression arg : iast.getArguments() ) {
            Object value = arg.accept(this, env);
            args.add(value);
        }
        return factory.newInstantiation(
                cd, args.toArray(new IASTexpression[0]));
    }
    
    public IASTexpression visit(IASTfieldRead iast, INormalizationEnvironment env)
            throws CompilationException {
        String fieldName = iast.getFieldName();
        IASTCclassDefinition cd = getClassByFieldName(fieldName);
        IASTexpression target = iast.getTarget().accept(this, env);
        return factory.newReadField(cd, fieldName, target);
    }

    public IASTexpression visit(IASTfieldWrite iast, INormalizationEnvironment env)
            throws CompilationException {
        String fieldName = iast.getFieldName();
        IASTCclassDefinition cd = getClassByFieldName(fieldName);
        IASTexpression target = iast.getTarget().accept(this, env);
        IASTexpression value = iast.getValue().accept(this, env);
        return factory.newWriteField(cd, fieldName, target, value);
    }

    public IASTexpression visit(IASTsend iast, INormalizationEnvironment env)
            throws CompilationException {
        String message = iast.getMethodName();
        IASTexpression receiver = iast.getReceiver().accept(this, env);
        IASTexpression[] arguments = iast.getArguments();
        IASTexpression[] args = new IASTexpression[arguments.length];
        for ( int i=0 ; i<arguments.length ; i++ ){
            IASTexpression argument = arguments[i];
            args[i] = argument.accept(this, env);
        }
        return factory.newSend(message, receiver, args);
    }

    public IASTexpression visit(IASTself iast, INormalizationEnvironment env)
            throws CompilationException {
        return env.renaming(iast);
    }

    public IASTexpression visit(IASTsuper iast, INormalizationEnvironment env)
            throws CompilationException {
        return factory.newSuper();
    }
}
