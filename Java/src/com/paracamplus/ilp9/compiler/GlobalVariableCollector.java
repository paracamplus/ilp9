package com.paracamplus.ilp9.compiler;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcomputedInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldRead;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldWrite;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCinstantiation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprimitiveInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
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

public class GlobalVariableCollector 
implements IASTCvisitor<Set<IASTCglobalVariable>, 
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
            IASTCglobalVariable gv = (IASTCglobalVariable) iast;
            if ( ! ( gv instanceof IASTClocalFunctionVariable ) ) {
                result = visit(gv, result);
            }
        }
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTCglobalFunctionVariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTCglobalVariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result.add(iast);
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTClocalFunctionVariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTClocalVariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTCvariable iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        throw new RuntimeException("Should not occur");
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
            IASTCcodefinitions iast,
            Set<IASTCglobalVariable> result)
                    throws CompilationException {
        return visit((IASTcodefinitions)iast, result);
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
            IASTCglobalInvocation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return visit((IASTinvocation) iast, result);
    }
    public Set<IASTCglobalVariable> visit(
            IASTClocalFunctionInvocation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return visit((IASTinvocation) iast, result);
    }
    public Set<IASTCglobalVariable> visit(
            IASTCcomputedInvocation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return visit((IASTinvocation) iast, result);
    }
    public Set<IASTCglobalVariable> visit(
            IASTCprimitiveInvocation iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return visit((IASTinvocation) iast, result);
    }
    
    public Set<IASTCglobalVariable> visit(
            IASTlambda iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getBody().accept(this, result);
        return result;
    }
    public Set<IASTCglobalVariable> visit(
            IASTClambda iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return visit((IASTlambda)iast, result);
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

    public Set<IASTCglobalVariable> visit(IASTinstantiation iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTCinstantiation ) {
            return visit((IASTCinstantiation) iast, result);
        } else {
            throw new CompilationException("should not occur");
        }
    }
    public Set<IASTCglobalVariable> visit(IASTCinstantiation iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        for ( IASTexpression expr : iast.getArguments() ) {
            result = expr.accept(this, result);
        }
        return result;
    }

    public Set<IASTCglobalVariable> visit(IASTfieldRead iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTCfieldRead ) {
            return visit((IASTCfieldRead) iast, result);
        } else {
            throw new RuntimeException("Should not occur");
        }
    }
    public Set<IASTCglobalVariable> visit(IASTCfieldRead iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getTarget().accept(this, result);
        return result;
    }

    public Set<IASTCglobalVariable> visit(IASTfieldWrite iast,
            Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        if ( iast instanceof IASTCfieldWrite ) {
            return visit((IASTCfieldWrite) iast, result);
        } else {
            throw new RuntimeException("Should not occur");
        }
    }
    public Set<IASTCglobalVariable> visit(IASTCfieldWrite iast,
                                          Set<IASTCglobalVariable> result)
        throws CompilationException {
        result = iast.getTarget().accept(this, result);
        result = iast.getValue().accept(this, result);
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(IASTsend iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        result = iast.getReceiver().accept(this, result);
        for ( IASTexpression expr : iast.getArguments() ) {
            result = expr.accept(this, result);
        }
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(IASTself iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
    
    public Set<IASTCglobalVariable> visit(IASTsuper iast,
                                          Set<IASTCglobalVariable> result) 
                    throws CompilationException {
        return result;
    }
}
