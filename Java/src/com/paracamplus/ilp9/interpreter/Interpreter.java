package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
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
import com.paracamplus.ilp9.interfaces.IASTreference;
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
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
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
    
    public Object visit(IASTbinding iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        throw new RuntimeException("Should never eval this!");
    }
    
    public Object visit(IASTboolean iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTclassDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTfloat iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTfunctionDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTinstantiation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTinteger iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTinvocation iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTlambda iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTloop iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTmethodDefinition iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASToperator iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTprogram iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getBody().accept(this, lexenv); // HACK
    }
    
    public Object visit(IASTreadField iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTreference iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        IASTvariable variable = iast.getVariable();
        try {
            return lexenv.getValue(variable);
        } catch (EvaluationException exc) {
            return getGlobalVariableEnvironment()
                    .getGlobalVariableValue(variable);
        }
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
    
    public Object visit(IASTsequence iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        IASTexpression[] expressions = iast.getExpressions();
        Object lastValue = null;
        for ( IASTexpression e : expressions ) {
            lastValue = e.accept(this, lexenv);
        }
        return lastValue;
    }
    
    public Object visit(IASTstring iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        return iast.getValue();
    }
    
    public Object visit(IASTsuper iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTtry iast, ILexicalEnvironment lexenv) 
            throws EvaluationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Object visit(IASTvariable iast, ILexicalEnvironment lexenv) 
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
