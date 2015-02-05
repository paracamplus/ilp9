package com.paracamplus.ilp9.ast;

import java.util.HashMap;
import java.util.Map;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTprogram extends AST implements IASTprogram {
    public ASTprogram(IASTfunctionDefinition[] functions,
                      IASTclassDefinition[] clazzes, 
                      IASTexpression expression) {
        this.functions = functions;
        this.clazzes = new HashMap<>();
        for (final IASTclassDefinition c : clazzes) {
            this.clazzes.put(c.getName(), c);
        }
        this.expression = expression;
    }
    protected IASTfunctionDefinition[] functions;
    protected Map<String, IASTclassDefinition> clazzes;
    protected IASTexpression expression;
    
    public IASTfunctionDefinition[] getFunctionDefinitions() {
        return functions;
    }
    public Map<String, IASTclassDefinition> getClassDefinitions() {
        return clazzes;
    }
    public IASTexpression getBody() {
        return this.expression;
    }

    public <Result, Data, Anomaly extends Throwable> 
    Result accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
            throws Anomaly {
        return visitor.visit(this, data);
    }
}
