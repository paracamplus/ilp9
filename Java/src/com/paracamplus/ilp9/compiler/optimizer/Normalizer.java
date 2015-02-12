package com.paracamplus.ilp9.compiler.optimizer;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.IOptimizer;
import com.paracamplus.ilp9.compiler.ast.ASTCLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionVariable;
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
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinstantiation;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTreadField;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;
import com.paracamplus.ilp9.interfaces.IASTwriteField;

public class Normalizer implements IOptimizer,
 IASTvisitor<IASTexpression, INormalizationEnvironment, CompilationException> {

    public Normalizer (INormalizationFactory factory) {
        this.factory = factory;
        this.globalVariables = new HashSet<>();
    }
    private final INormalizationFactory factory;
    private final Set<IASTvariable> globalVariables;

    public IASTCprogram transform(IASTCprogram program) 
            throws CompilationException {
        
        Map<String, IASTclassDefinition> clazzes = 
                program.getClassDefinitions();

        IASTfunctionDefinition[] functions = program.getFunctionDefinitions();
        IASTfunctionDefinition[] funs = 
                new IASTfunctionDefinition[functions.length];
        INormalizationEnvironment env = NormalizationEnvironment.EMPTY;
        for ( IASTfunctionDefinition function : functions ) {
            IASTCGlobalFunctionVariable gfv =
                    factory.newGlobalFunctionVariable(function.getName());
            env = env.extend(gfv, gfv);
        }
        for ( int i=0 ; i<functions.length ; i++ ) {
            IASTfunctionDefinition function = functions[i];
            IASTfunctionDefinition newfunction = visit(function, env);
            funs[i] = newfunction;
        }
        
        IASTexpression body = program.getBody();
        IASTexpression newbody = body.accept(this, env);
        return factory.newProgram(funs, clazzes, newbody);
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
        IASTbinding[] newbindings = new IASTbinding[bindings.length];
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
        IASTfunctionDefinition[] functions = iast.getFunctions();
        IASTfunctionDefinition[] newfunctions = 
                new IASTfunctionDefinition[functions.length];
        INormalizationEnvironment bodyenv = env;
        for ( int i=0 ; i<functions.length ; i++ ) {
            IASTfunctionDefinition function = functions[i];
            String functionName = function.getName();
            IASTvariable oldFunctionName = // HACK
                    new com.paracamplus.ilp9.ast.ASTvariable(functionName);
            IASTvariable newFunctionName = 
                    factory.newLocalVariable(functionName);
            INormalizationEnvironment newenv = env;
            IASTvariable[] variables = function.getVariables();
            IASTvariable[] newvariables = new IASTvariable[variables.length];
            for ( int iv=0 ; iv<variables.length ; iv++ ) {
                IASTvariable variable = variables[i];
                IASTvariable newvariable = 
                        factory.newLocalVariable(variable.getName());
                newvariables[i] = newvariable;
                newenv = newenv.extend(variable, newvariable);
            }
            IASTexpression newbody = function.getBody().accept(this, newenv);
            newfunctions[i] = factory.newFunctionDefinition(
                    newFunctionName.getName(), newvariables, newbody);
            bodyenv = bodyenv.extend(oldFunctionName, newFunctionName);
        }
        IASTexpression newbody = iast.getBody().accept(this, bodyenv);
        return factory.newCodefinitions(newfunctions, newbody);
    }

    public IASTfunctionDefinition visit(
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
        return factory.newFunctionDefinition(functionName, newvariables, newbody);
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
        if ( funexpr instanceof IASTCGlobalVariable ) {
            IASTCGlobalVariable f = (IASTCGlobalVariable) funexpr;
            return factory.newGlobalInvocation(f, args);
        } else if ( funexpr instanceof IASTCLocalFunctionVariable ) {
            IASTCLocalFunctionVariable f = (ASTCLocalFunctionVariable) funexpr;
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

    public IASTexpression visit(IASTreadField iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public IASTexpression visit(IASTinstantiation iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public IASTexpression visit(IASTself iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public IASTexpression visit(IASTsend iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public IASTexpression visit(IASTsuper iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public IASTexpression visit(IASTwriteField iast, INormalizationEnvironment env)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

}
