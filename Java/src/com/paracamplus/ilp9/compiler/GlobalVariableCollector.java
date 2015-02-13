package com.paracamplus.ilp9.compiler;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
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
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
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
implements IASTvisitor<Set<IASTCglobalVariable>, 
                       Set<IASTCglobalVariable>, 
                       CompilationException> {

    public GlobalVariableCollector () {
        this.result = new HashSet<>();
    }
    private Set<IASTCglobalVariable> result;
    
    public Set<IASTCglobalVariable> analyze(IASTprogram program) 
            throws CompilationException {
        for ( IASTfunctionDefinition ifd : program.getFunctionDefinitions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = program.getBody().accept(this, result);
        return result;
    }

    public Set<IASTCglobalVariable> visit(
            IASTvariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTCglobalVariable ) {
            if ( ! (iast instanceof IASTCglobalFunctionVariable ) ) { 
                IASTCglobalVariable gv = (IASTCglobalVariable) iast;
                result.add(gv);
            }
        }
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTalternative iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getConsequence().accept(this, result);
        result = iast.getAlternant().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTassignment iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getVariable().accept(this, result);
        result = iast.getExpression().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTbinaryOperation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        iast.getLeftOperand().accept(this, result);
        iast.getRightOperand().accept(this, result);
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTunaryOperation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        iast.getOperand().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTboolean iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTfloat iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTinteger iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTstring iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }

    public Set<IASTCglobalVariable> visit(
            IASTblock iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        for ( IASTbinding binding : iast.getBindings() ) {
            result = binding.getInitialisation().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
       
    public Set<IASTCglobalVariable> visit(
            IASTcodefinitions iast,
            Set<IASTCglobalVariable> result)
                    throws CompilationException {
        for ( IASTnamedLambda ifd : iast.getFunctions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTinvocation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getFunction().accept(this, result);
        for ( IASTexpression arg : iast.getArguments() ) {
            result = arg.accept(this, result);
        }
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTlambda iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTloop iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASToperator iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTsequence iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        for ( IASTexpression expr : iast.getExpressions() ) {
            result = expr.accept(this, result);
        }
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTtry iast,
            Set<IASTCglobalVariable> result) 
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
    
    public Set<IASTCglobalVariable> visit(IASTwriteField iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCglobalVariable> visit(IASTsuper iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCglobalVariable> visit(IASTsend iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public Set<IASTCglobalVariable> visit(IASTreadField iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }

    public Set<IASTCglobalVariable> visit(IASTinstantiation iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(IASTself iast,
            Set<IASTCglobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }




}
