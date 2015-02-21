package com.paracamplus.ilp9.interpreter;

import java.util.List;
import java.util.Vector;

import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
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
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
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
import com.paracamplus.ilp9.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp9.interpreter.interfaces.IFunction;
import com.paracamplus.ilp9.interpreter.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IOperator;
import com.paracamplus.ilp9.interpreter.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.Invocable;
import com.paracamplus.ilp9.interpreter.primitive.Throw.ThrownException;

public class Interpreter
implements IASTvisitor<Object, ILexicalEnvironment, EvaluationException> {
    
    public Interpreter (IGlobalVariableEnvironment globalVariableEnvironment,
                        IOperatorEnvironment operatorEnvironment) {
        this.globalVariableEnvironment = globalVariableEnvironment;
        this.operatorEnvironment = operatorEnvironment;
    }
    protected IGlobalVariableEnvironment globalVariableEnvironment;
    protected IOperatorEnvironment operatorEnvironment;

    public IOperatorEnvironment getOperatorEnvironment() {
        return operatorEnvironment;
    }
    
    public IGlobalVariableEnvironment getGlobalVariableEnvironment() {
        return globalVariableEnvironment;
    }

    // 
    
    public Object visit(IASTprogram iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        for ( IASTfunctionDefinition fd : iast.getFunctionDefinitions() ) {
            Object f = this.visit(fd, lexenv);
            String v = fd.getName();
            getGlobalVariableEnvironment().addGlobalVariableValue(v, f);
        }
        try {
            return iast.getBody().accept(this, lexenv);
        } catch (ThrownException exc) {
            return exc.getThrownValue();
        } catch (Exception exc) {
            return exc;
        }
    }
   
    // 
    
    private static Object whatever = "whatever";
            
    public Object visit(IASTalternative iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Object c = iast.getCondition().accept(this, lexenv);
        if ( c != null && c instanceof Boolean ) {
            Boolean b = (Boolean) c;
            if ( b.booleanValue() ) {
                return iast.getConsequence().accept(this, lexenv);
            } else if ( iast.isTernary() ) {
                return iast.getAlternant().accept(this, lexenv);                
            } else {
                return whatever;
            }
        } else {
            return iast.getConsequence().accept(this, lexenv);
        }
    }
    
    public Object visit(IASTassignment iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        IASTvariable variable = iast.getVariable();
        Object value = iast.getExpression().accept(this, lexenv);
        try {
            lexenv.update(variable, value);
        } catch (EvaluationException exc) {
            getGlobalVariableEnvironment()
                .updateGlobalVariableValue(variable.getName(), value);
        }
        return value;
    }

    public Object visit(IASTunaryOperation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Object operand = iast.getOperand().accept(this, lexenv);
        IASToperator operator = iast.getOperator();
        IOperator op = getOperatorEnvironment().getUnaryOperator(operator);
        return op.apply(operand);
    }
    
    public Object visit(IASTbinaryOperation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Object leftOperand = iast.getLeftOperand().accept(this, lexenv);
        Object rightOperand = iast.getRightOperand().accept(this, lexenv);
        IASToperator operator = iast.getOperator();
        IOperator op = getOperatorEnvironment().getBinaryOperator(operator);
        return op.apply(leftOperand, rightOperand);
    }

    public Object visit(IASToperator iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        throw new RuntimeException("Value-less operator");
    }

    public Object visit(IASTsequence iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        IASTexpression[] expressions = iast.getExpressions();
        Object lastValue = null;
        for ( IASTexpression e : expressions ) {
            lastValue = e.accept(this, lexenv);
        }
        return lastValue;
    }
    
    public Object visit(IASTblock iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        ILexicalEnvironment lexenv2 = lexenv;
        for ( IASTbinding binding : iast.getBindings() ) {
            Object initialisation = 
                    binding.getInitialisation().accept(this, lexenv);
            lexenv2 = lexenv2.extend(binding.getVariable(), initialisation);
        }
        return iast.getBody().accept(this, lexenv2);
    }

    public Object visit(IASTboolean iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTinteger iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTfloat iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTstring iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }

    public Object visit(IASTvariable iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        try {
            return lexenv.getValue(iast);
        } catch (EvaluationException exc) {
            return getGlobalVariableEnvironment()
                    .getGlobalVariableValue(iast.getName());
        }
    }
    
    public Object visit(IASTfunctionDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Invocable fun = new Function(iast.getVariables(),
                                     iast.getBody(),
                                     LexicalEnvironment.EMPTY);
        getGlobalVariableEnvironment()
            .addGlobalVariableValue(iast.getName(), fun);
        return fun;
    }
    
    public Object visit(IASTinvocation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Object function = iast.getFunction().accept(this, lexenv);
        if ( function instanceof Invocable ) {
            Invocable f = (Invocable)function;
            List<Object> args = new Vector<Object>();
            for ( IASTexpression arg : iast.getArguments() ) {
                Object value = arg.accept(this, lexenv);
                args.add(value);
            }
            return f.apply(this, args.toArray());
        } else {
            String msg = "Cannot apply " + function;
            throw new EvaluationException(msg);
        }
    }
    
    public Object visit(IASTlambda iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        IFunction fun = new Function(iast.getVariables(),
                                     iast.getBody(),
                                     lexenv);
        return fun;
    }
    
    public Object visit(IASTcodefinitions iast, ILexicalEnvironment lexenv)
            throws EvaluationException {
        IASTnamedLambda[] functions = iast.getFunctions();
        ILexicalEnvironment lexenv2 = lexenv;
        for ( IASTnamedLambda fun : functions ) {
            IASTvariable variable = fun.getFunctionVariable();
            lexenv2 = lexenv2.extend(variable, null);
        }
        for ( IASTnamedLambda fun : functions ) {
            Object f = this.visit(fun, lexenv2);
            IASTvariable variable = fun.getFunctionVariable();
            lexenv2.update(variable, f);
        }
        IASTexpression body = iast.getBody();
        Object result = body.accept(this, lexenv2);
        return result;
    }
    
    public Object visit(IASTloop iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        while ( true ) {
            Object condition = iast.getCondition().accept(this, lexenv);
            if ( condition instanceof Boolean ) {
                Boolean c = (Boolean) condition;
                if ( ! c ) {
                    break;
                }
            }
            iast.getBody().accept(this, lexenv);
        }
        return Boolean.FALSE;
    }

    public Object visit(IASTtry iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        Object result = Boolean.FALSE;
        IFunction fcatcher = null;
        IASTlambda catcher = iast.getCatcher();
        if ( null != catcher ) {
            fcatcher = (IFunction) catcher.accept(this, lexenv);
        }
        try {
            result = iast.getBody().accept(this, lexenv);
        } catch (ThrownException exc) {
            if ( null != fcatcher ) {
                Object value = exc.getThrownValue();
                fcatcher.apply(this, new Object[]{ value });
            } else {
                throw exc;
            }
        } catch (EvaluationException exc) {
            if ( null != fcatcher ) {
                fcatcher.apply(this, new Object[]{ exc });
            } else {
                throw exc;
            }
        } catch (Exception exc) {
            if ( null != fcatcher ) {
                EvaluationException e = new EvaluationException(exc);
                fcatcher.apply(this, new Object[]{ e });
            } else {
                throw exc;
            }
        } finally {
            IASTexpression finallyer = iast.getFinallyer();
            if ( null != finallyer ) {
                finallyer.accept(this, lexenv);
            }
        }
        return result;
    }

    // Class-related methods 
    
    public Object visit(IASTclassDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTmethodDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public Object visit(IASTinstantiation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }    
     
    public Object visit(IASTreadField iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTself iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTsend iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
     public Object visit(IASTsuper iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
            
    public Object visit(IASTwriteField iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
}
