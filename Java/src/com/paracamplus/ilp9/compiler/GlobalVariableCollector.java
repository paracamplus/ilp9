package com.paracamplus.ilp9.compiler;

import java.util.List;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.ast.IASTCprogram;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalVariable;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
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
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;

public class GlobalVariableCollector 
implements IASTvisitor<List<IASTGlobalVariable>, 
                       List<IASTGlobalVariable>, 
                       CompilationException>{

    public GlobalVariableCollector () {
        this.result = new Vector<>();
    }
    private List<IASTGlobalVariable> result;
    
    public void analyze(IASTCprogram program) 
            throws CompilationException {
        for ( IASTfunctionDefinition ifd : program.getFunctionDefinitions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = program.getBody().accept(this, result);
        program.setGlobalVariables(result);
    }

    public List<IASTGlobalVariable> visit(
            IASTvariable iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTGlobalVariable ) {
            if ( ! (iast instanceof IASTGlobalFunctionVariable ) ) { 
                IASTGlobalVariable gv = (IASTGlobalVariable) iast;
                result.add(gv);
            }
        }
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTalternative iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getConsequence().accept(this, result);
        result = iast.getAlternant().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTassignment iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getVariable().accept(this, result);
        result = iast.getExpression().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTbinaryOperation iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        iast.getLeftOperand().accept(this, result);
        iast.getRightOperand().accept(this, result);
        return result;
    }
    public List<IASTGlobalVariable> visit(
            IASTunaryOperation iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        iast.getOperand().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTboolean iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public List<IASTGlobalVariable> visit(
            IASTfloat iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public List<IASTGlobalVariable> visit(
            IASTinteger iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public List<IASTGlobalVariable> visit(
            IASTstring iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }

    public List<IASTGlobalVariable> visit(
            IASTblock iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        for ( IASTbinding binding : iast.getBindings() ) {
            result = binding.getInitialisation().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
       
    public List<IASTGlobalVariable> visit(
            IASTcodefinitions iast,
            List<IASTGlobalVariable> result)
                    throws CompilationException {
        for ( IASTfunctionDefinition ifd : iast.getFunctions() ) {
            result = ifd.getBody().accept(this, result);
        }
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTinvocation iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getFunction().accept(this, result);
        for ( IASTexpression arg : iast.getArguments() ) {
            result = arg.accept(this, result);
        }
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTlambda iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTloop iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        result = iast.getCondition().accept(this, result);
        result = iast.getBody().accept(this, result);
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASToperator iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTsequence iast,
            List<IASTGlobalVariable> result) 
                    throws CompilationException {
        for ( IASTexpression expr : iast.getExpressions() ) {
            result = expr.accept(this, result);
        }
        return result;
    }
    
    public List<IASTGlobalVariable> visit(
            IASTtry iast,
            List<IASTGlobalVariable> result) 
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
    
    public List<IASTGlobalVariable> visit(IASTwriteField iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public List<IASTGlobalVariable> visit(IASTsuper iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public List<IASTGlobalVariable> visit(IASTsend iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    public List<IASTGlobalVariable> visit(IASTreadField iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }

    public List<IASTGlobalVariable> visit(IASTinstantiation iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }
    
    public List<IASTGlobalVariable> visit(IASTself iast,
            List<IASTGlobalVariable> result) throws CompilationException {
        // TODO Auto-generated method stub
        return result;
    }




}
