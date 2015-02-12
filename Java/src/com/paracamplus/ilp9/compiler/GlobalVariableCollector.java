package com.paracamplus.ilp9.compiler;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
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

public class GlobalVariableCollector 
implements IASTvisitor<Set<IASTCGlobalVariable>, 
                       Set<IASTCGlobalVariable>, 
                       CompilationException> {

    public GlobalVariableCollector () {
        this.result = new HashSet<>();
    }
    private Set<IASTCGlobalVariable> result;
    
    public Set<IASTCGlobalVariable> analyze(IASTprogram program) 
            throws CompilationException {
        for ( IASTfunctionDefinition ifd : program.getFunctionDefinitions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = program.getBody().accept(this, result);
        return result;
    }

    public Set<IASTCGlobalVariable> visit(
            IASTvariable iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTCGlobalVariable ) {
            if ( ! (iast instanceof IASTCGlobalFunctionVariable ) ) { 
                IASTCGlobalVariable gv = (IASTCGlobalVariable) iast;
                result.add(gv);
            }
        }
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTalternative iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getConsequence().accept(this, result);
        result = iast.getAlternant().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTassignment iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getVariable().accept(this, result);
        result = iast.getExpression().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTbinaryOperation iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        iast.getLeftOperand().accept(this, result);
        iast.getRightOperand().accept(this, result);
        return result;
    }
    public Set<IASTCGlobalVariable> visit(
            IASTunaryOperation iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        iast.getOperand().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTboolean iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCGlobalVariable> visit(
            IASTfloat iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCGlobalVariable> visit(
            IASTinteger iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCGlobalVariable> visit(
            IASTstring iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }

    public Set<IASTCGlobalVariable> visit(
            IASTblock iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        for ( IASTbinding binding : iast.getBindings() ) {
            result = binding.getInitialisation().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
       
    public Set<IASTCGlobalVariable> visit(
            IASTcodefinitions iast,
            Set<IASTCGlobalVariable> result)
                    throws CompilationException {
        for ( IASTfunctionDefinition ifd : iast.getFunctions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTinvocation iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getFunction().accept(this, result);
        for ( IASTexpression arg : iast.getArguments() ) {
            result = arg.accept(this, result);
        }
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTlambda iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTloop iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASToperator iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTsequence iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        for ( IASTexpression expr : iast.getExpressions() ) {
            result = expr.accept(this, result);
        }
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(
            IASTtry iast,
            Set<IASTCGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getBody().accept(this, result);
        IASTlambda catcher = iast.getCatcher();
        if ( catcher != null ) {
            result = catcher.accept(this, result);
        }
        IASTexpression finallyer = iast.getFinallyer();
        if ( finallyer != null ) {
            result = finallyer.accept(this, result);
        }
        return result;
    }
    
    // Class related
    
    public Set<IASTCGlobalVariable> visit(IASTwriteField iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCGlobalVariable> visit(IASTsuper iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCGlobalVariable> visit(IASTsend iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCGlobalVariable> visit(IASTreadField iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }

    public Set<IASTCGlobalVariable> visit(IASTinstantiation iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    
    public Set<IASTCGlobalVariable> visit(IASTself iast,
            Set<IASTCGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }




}
