/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.interfaces.IASTCblock;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcomputedInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldRead;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldWrite;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCinstantiation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprimitiveInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
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
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class FreeVariableCollector 
implements IASTCvisitor<Void, Set<IASTClocalVariable>, CompilationException> {

    public FreeVariableCollector(IASTCprogram program) {
        this.program = program;
    }
    protected final IASTCprogram program;
    
    public IASTCprogram analyze () 
            throws CompilationException {
        for ( IASTfunctionDefinition ifd : program.getFunctionDefinitions() ) {
            Set<IASTClocalVariable> newvars = new HashSet<>();
            visit(ifd, newvars);
        }
        Set<IASTClocalVariable> newvars = new HashSet<>();
        program.getBody().accept(this, newvars);
        return program;
    }
    
    public Void visit(IASTvariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        if ( iast instanceof IASTClocalVariable ) {
            variables.add((IASTClocalVariable)iast);
        }
        return null;
    }
    public Void visit(IASTCglobalFunctionVariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        return null;
    }
    public Void visit(IASTCglobalVariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        return null;
    }
    public Void visit(IASTClocalFunctionVariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        return null;
    }
    public Void visit(IASTClocalVariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        variables.add(iast);
        return null;
    }
    public Void visit(IASTCvariable iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        return null;
    }
    
    public Void visit(IASTassignment iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        IASTvariable variable = iast.getVariable();
        variable.accept(this, variables);
        iast.getExpression().accept(this, variables);
        try {
            // Cast ensured by normalizer:
            ((IASTCvariable) variable).setMutable();
        } catch (ClassCastException exc) {
            throw new RuntimeException("should never occur!");
        }
        return null;
    }
    
    public Void visit(IASTalternative iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        iast.getCondition().accept(this, variables);
        iast.getConsequence().accept(this, variables);
        iast.getAlternant().accept(this, variables);
        return null;
    }
    
    public Void visit(IASTbinaryOperation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        iast.getLeftOperand().accept(this, variables);
        iast.getRightOperand().accept(this, variables);
        return null;
    }
    public Void visit(IASTunaryOperation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        iast.getOperand().accept(this, variables);
        return null;
    }
    public Void visit(IASToperator iast, Set<IASTClocalVariable> variables) throws CompilationException {
        throw new RuntimeException("Should not occur");
    }
    
    public Void visit(IASTboolean iast, Set<IASTClocalVariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTfloat iast, Set<IASTClocalVariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTinteger iast, Set<IASTClocalVariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTstring iast, Set<IASTClocalVariable> variables) throws CompilationException {
        return null;
    }
    
    public Void visit(IASTinvocation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        iast.getFunction().accept(this, variables);
        for ( IASTexpression expression : iast.getArguments() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    public Void visit(IASTCcomputedInvocation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTinvocation) iast, variables);
    }
    public Void visit(IASTCprimitiveInvocation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTinvocation) iast, variables);
    }
    public Void visit(IASTCglobalInvocation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTinvocation) iast, variables);
    }
    public Void visit(IASTClocalFunctionInvocation iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTinvocation) iast, variables);
    }
    
    public Void visit(IASTloop iast, Set<IASTClocalVariable> variables) throws CompilationException {
        iast.getCondition().accept(this, variables);
        iast.getBody().accept(this, variables);
        return null;
    }
    
    public Void visit(IASTsequence iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        for ( IASTexpression expression : iast.getExpressions() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    
    public Void visit(IASTtry iast, Set<IASTClocalVariable> variables) throws CompilationException {
        iast.getBody().accept(this, variables);
        IASTlambda catcher = iast.getCatcher();
        if ( catcher != null ) {
            Set<IASTClocalVariable> newvars = new HashSet<>();
            catcher.getBody().accept(this, newvars);
            newvars.remove(catcher.getVariables()[0]);
            variables.addAll(newvars);
        }
        IASTexpression finallyer = iast.getFinallyer();
        if ( finallyer != null ) {
            finallyer.accept(this, variables);
        }
        return null;
    }
    
    public Void visit(IASTfunctionDefinition fd, 
            Set<IASTClocalVariable> variables) 
            throws CompilationException {
        Set<IASTClocalVariable> newvars = new HashSet<>();
        fd.getBody().accept(this, newvars);
        IASTvariable[] vars = fd.getVariables();
        newvars.removeAll(Arrays.asList(vars));
        try {
            // Cast ensured by normalizer:
            IASTCfunctionDefinition fun = (IASTCfunctionDefinition) fd;
            fun.setClosedVariables(newvars);
            for ( IASTvariable v : newvars ) {
                ((IASTClocalVariable)v).setClosed();
            }
        } catch (ClassCastException exc) {
            throw new RuntimeException("should not occur");
        }
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTblock iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        if ( iast instanceof IASTCblock ) {
            return visit((IASTCblock) iast, variables);
        } else {
            throw new RuntimeException("should not occur");
        }
    }
    public Void visit(IASTCblock iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        Set<IASTClocalVariable> currentVars = new HashSet<>();
        for ( IASTCblock.IASTCbinding binding : iast.getBindings() ) {
            binding.getInitialisation().accept(this, variables);
            currentVars.add(binding.getVariable());
        }
        Set<IASTClocalVariable> newvars = new HashSet<>();
        iast.getBody().accept(this, newvars);
        newvars.removeAll(currentVars);
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTlambda iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        Set<IASTClocalVariable> newvars = new HashSet<>();
        iast.getBody().accept(this, newvars);
        IASTvariable[] vars = iast.getVariables();
        newvars.removeAll(Arrays.asList(vars));
        try {
            // Cast ensured by normalizer:
            IASTClambda f = (IASTClambda) iast;
            f.setClosedVariables(newvars);
            program.addClosureDefinition(f);
            for ( IASTvariable v : newvars ) {
                // Cast ensured by normalizer:
                ((IASTClocalVariable)v).setClosed();
            }
        } catch (ClassCastException exc) {
            throw new RuntimeException("should not occur");
        }
        variables.addAll(newvars);
        return null;
    }
    public Void visit(IASTClambda iast, Set<IASTClocalVariable> variables) 
            throws CompilationException {
        Set<IASTClocalVariable> newvars = new HashSet<>();
        iast.getBody().accept(this, newvars);
        IASTvariable[] vars = iast.getVariables();
        newvars.removeAll(Arrays.asList(vars));
        try {
            iast.setClosedVariables(newvars);
            program.addClosureDefinition(iast);
            for ( IASTvariable v : newvars ) {
                // Cast ensured by normalizer:
                ((IASTClocalVariable)v).setClosed();
            }
        } catch (ClassCastException exc) {
            throw new RuntimeException("should not occur");
        }
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTcodefinitions iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        IASTnamedLambda[] functions = iast.getFunctions();
        // Collect the names of the local functions:
        List<IASTvariable> functionsVariables = new Vector<>(); 
        for ( IASTnamedLambda ifd : functions ) {
            functionsVariables.add(ifd.getFunctionVariable());
        }
        for ( IASTnamedLambda ifd : functions ) {
            Set<IASTClocalVariable> newvars = new HashSet<>();
            visit(ifd, newvars);
            IASTvariable[] vars = ifd.getVariables();
            newvars.removeAll(Arrays.asList(vars));
            try {
                // Cast ensured by normalizer:
                IASTClambda fun = (IASTClambda) ifd;
                fun.setClosedVariables(newvars);
                for ( IASTvariable v : newvars ) {
                    // Cast ensured by normalizer:
                    ((IASTClocalVariable)v).setClosed();
                }
            } catch (ClassCastException exc) {
                throw new RuntimeException("should not occur");
            }
            newvars.removeAll(functionsVariables);
            variables.addAll(newvars);
        }
        for ( IASTnamedLambda ifd : functions ) {
            try {
                // Cast ensured by normalizer:
                IASTClocalFunctionVariable v = 
                    (IASTClocalFunctionVariable) ifd.getFunctionVariable();
                v.setClosed();
            } catch (ClassCastException exc) {
                throw new RuntimeException("should not occur");
            }
        }
        iast.getBody().accept(this, variables);
        variables.removeAll(functionsVariables);
        return null;
    }
    public Void visit(IASTCcodefinitions iast, Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTcodefinitions) iast, variables); 
    }

    // Class related 
    
    public Void visit(IASTinstantiation iast, 
                      Set<IASTClocalVariable> variables)
                    throws CompilationException {
        for ( IASTexpression expression : iast.getArguments() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    public Void visit(IASTCinstantiation iast, 
                      Set<IASTClocalVariable> variables)
            throws CompilationException {
       return visit((IASTinstantiation)iast, variables);
    }

    public Void visit(IASTfieldRead iast, 
            Set<IASTClocalVariable> variables)
                    throws CompilationException {
        iast.getTarget().accept(this, variables);
        return null;
    }
    public Void visit(IASTCfieldRead iast, 
                      Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTfieldRead)iast, variables);
    }

    public Void visit(IASTfieldWrite iast, 
            Set<IASTClocalVariable> variables)
                    throws CompilationException {
        iast.getTarget().accept(this, variables);
        iast.getValue().accept(this, variables);
        return null;
    }
    public Void visit(IASTCfieldWrite iast, 
                      Set<IASTClocalVariable> variables)
            throws CompilationException {
        return visit((IASTfieldWrite)iast, variables);
    }
    
    public Void visit(IASTsend iast, 
                      Set<IASTClocalVariable> variables) 
            throws CompilationException {
        iast.getReceiver().accept(this, variables);
        for ( IASTexpression expression : iast.getArguments() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    
    public Void visit(IASTself iast, 
                      Set<IASTClocalVariable> variables) 
        throws CompilationException {
        return null;
    }
    
    public Void visit(IASTsuper iast, 
                      Set<IASTClocalVariable> variables) 
        throws CompilationException {
        return null;
    }
}
