package com.paracamplus.ilp9.compiler;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
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
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
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

public class FreeVariableCollector 
implements IASTvisitor<Void, Set<IASTvariable>, CompilationException> {

    public FreeVariableCollector(IASTCprogram program) {
        this.program = program;
    }
    protected final IASTCprogram program;
    
    public IASTCprogram analyze () 
            throws CompilationException {
        for ( IASTfunctionDefinition ifd : program.getFunctionDefinitions() ) {
            Set<IASTvariable> newvars = new HashSet<>();
            visit(ifd, newvars);
        }
        Set<IASTvariable> newvars = new HashSet<>();
        program.getBody().accept(this, newvars);
        return program;
    }
    
    public Void visit(IASTvariable iast, Set<IASTvariable> variables) throws CompilationException {
        if ( iast instanceof IASTCLocalVariable ) {
            variables.add(iast);
        }
        return null;
    }
    
    public Void visit(IASTassignment iast, Set<IASTvariable> variables)
            throws CompilationException {
        IASTvariable variable = iast.getVariable();
        variable.accept(this, variables);
        iast.getExpression().accept(this, variables);
        try {
            ((IASTCvariable) variable).setMutable();
        } catch (ClassCastException exc) {
            // should never occur!
        }
        return null;
    }
    
    public Void visit(IASTalternative iast, Set<IASTvariable> variables)
            throws CompilationException {
        iast.getCondition().accept(this, variables);
        iast.getConsequence().accept(this, variables);
        iast.getAlternant().accept(this, variables);
        return null;
    }
    
    public Void visit(IASTbinaryOperation iast, Set<IASTvariable> variables)
            throws CompilationException {
        iast.getLeftOperand().accept(this, variables);
        iast.getRightOperand().accept(this, variables);
        return null;
    }
    public Void visit(IASTunaryOperation iast, Set<IASTvariable> variables)
            throws CompilationException {
        iast.getOperand().accept(this, variables);
        return null;
    }
    public Void visit(IASToperator iast, Set<IASTvariable> variables) throws CompilationException {
        throw new RuntimeException("Should not occur");
    }
    
    public Void visit(IASTboolean iast, Set<IASTvariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTfloat iast, Set<IASTvariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTinteger iast, Set<IASTvariable> variables) throws CompilationException {
        return null;
    }
    public Void visit(IASTstring iast, Set<IASTvariable> variables) throws CompilationException {
        return null;
    }
    
    public Void visit(IASTinvocation iast, Set<IASTvariable> variables)
            throws CompilationException {
        iast.getFunction().accept(this, variables);
        for ( IASTexpression expression : iast.getArguments() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    
    public Void visit(IASTloop iast, Set<IASTvariable> variables) throws CompilationException {
        iast.getCondition().accept(this, variables);
        iast.getBody().accept(this, variables);
        return null;
    }
    
    public Void visit(IASTsequence iast, Set<IASTvariable> variables) 
            throws CompilationException {
        for ( IASTexpression expression : iast.getExpressions() ) {
            expression.accept(this, variables);
        }
        return null;
    }
    
    public Void visit(IASTtry iast, Set<IASTvariable> variables) throws CompilationException {
        iast.getBody().accept(this, variables);
        IASTlambda catcher = iast.getCatcher();
        if ( catcher != null ) {
            //catcher.accept(this, variables);
            Set<IASTvariable> newvars = new HashSet<>();
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
            Set<IASTvariable> variables) 
            throws CompilationException {
        Set<IASTvariable> newvars = new HashSet<>();
        fd.getBody().accept(this, newvars);
        IASTvariable[] vars = fd.getVariables();
        newvars.removeAll(Arrays.asList(vars));
        try {
            IASTCfunctionDefinition fun = (IASTCfunctionDefinition) fd;
            fun.setClosedVariables(newvars);
            for ( IASTvariable v : newvars ) {
                ((IASTCLocalVariable)v).setClosed();
            }
        } catch (ClassCastException exc) {
            // should not occur
        }
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTblock iast, Set<IASTvariable> variables) 
            throws CompilationException {
        Set<IASTvariable> currentVars = new HashSet<>();
        for ( IASTblock.IASTbinding binding : iast.getBindings() ) {
            binding.getInitialisation().accept(this, variables);
            currentVars.add(binding.getVariable());
        }
        Set<IASTvariable> newvars = new HashSet<>();
        iast.getBody().accept(this, newvars);
        newvars.removeAll(currentVars);
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTlambda iast, Set<IASTvariable> variables) throws CompilationException {
        Set<IASTvariable> newvars = new HashSet<>();
        iast.getBody().accept(this, newvars);
        IASTvariable[] vars = iast.getVariables();
        newvars.removeAll(Arrays.asList(vars));
        try {
            IASTClambda f = (IASTClambda) iast;
            f.setClosedVariables(newvars);
            program.addClosureDefinition(f);
            for ( IASTvariable v : newvars ) {
                ((IASTCLocalVariable)v).setClosed();
            }
        } catch (ClassCastException exc) {
            throw new RuntimeException("should not occur");
        }
        variables.addAll(newvars);
        return null;
    }
    
    public Void visit(IASTcodefinitions iast, Set<IASTvariable> variables)
            throws CompilationException {
        IASTnamedLambda[] functions = iast.getFunctions();
        // Collect the names of the local functions:
        List<IASTvariable> functionsVariables = new Vector<>(); 
        for ( IASTnamedLambda ifd : functions ) {
            functionsVariables.add(ifd.getFunctionVariable());
        }
        for ( IASTnamedLambda ifd : functions ) {
            Set<IASTvariable> newvars = new HashSet<>();
            visit(ifd, newvars);
            IASTvariable[] vars = ifd.getVariables();
            newvars.removeAll(Arrays.asList(vars));
            try {
                IASTClambda fun = (IASTClambda) ifd;
                fun.setClosedVariables(newvars);
                for ( IASTvariable v : newvars ) {
                    ((IASTCLocalVariable)v).setClosed();
                }
            } catch (ClassCastException exc) {
                throw new RuntimeException("should not occur");
            }
            newvars.removeAll(functionsVariables);
            variables.addAll(newvars);
        }
        for ( IASTnamedLambda ifd : functions ) {
            try {
                IASTCLocalFunctionVariable v = 
                    (IASTCLocalFunctionVariable) ifd.getFunctionVariable();
                v.setClosed();
            } catch (ClassCastException exc) {
                throw new RuntimeException("should not occur");
            }
        }
        iast.getBody().accept(this, variables);
        variables.removeAll(functionsVariables);
        return null;
    }

    // Class related 
    
    public Void visit(IASTwriteField iast, Set<IASTvariable> variables)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTinstantiation iast, Set<IASTvariable> variables)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTreadField iast, Set<IASTvariable> variables)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTself iast, Set<IASTvariable> variables) throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTsend iast, Set<IASTvariable> variables) throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTsuper iast, Set<IASTvariable> variables) throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
}
