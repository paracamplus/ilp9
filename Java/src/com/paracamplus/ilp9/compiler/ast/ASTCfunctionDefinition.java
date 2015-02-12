package com.paracamplus.ilp9.compiler.ast;

import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp9.ast.ASTfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class ASTCfunctionDefinition extends ASTfunctionDefinition 
implements IASTCfunctionDefinition {

    public ASTCfunctionDefinition (String functionName,
                                   IASTvariable[] variables,
                                   IASTexpression body ) {
        super(functionName, variables, body);
        this.closedVariables = new HashSet<>();
    }
    private final Set<IASTvariable> closedVariables;

    public Set<IASTvariable> getClosedVariables() {
        return closedVariables;
    }

    public void setClosedVariables(Set<IASTvariable> closedVariables) {
        this.closedVariables.addAll(closedVariables);
    }
}
