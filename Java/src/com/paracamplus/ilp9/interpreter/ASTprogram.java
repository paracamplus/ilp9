package com.paracamplus.ilp9.interpreter;

import java.util.HashMap;
import java.util.Map;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTprogram;

public class ASTprogram extends AST implements IASTprogram {
    public ASTprogram(IASTfunctionDefinition[] functions,
                      IASTclassDefinition[] clazzes, 
                      IASTexpression expression) {
        this.functions = new HashMap<>();
        for (final IASTfunctionDefinition f : functions) {
            this.functions.put(f.getName(), f);
        }
        this.clazzes = new HashMap<>();
        for (final IASTclassDefinition c : clazzes) {
            this.clazzes.put(c.getName(), c);
        }
        this.expression = expression;
    }
    protected Map<String, IASTfunctionDefinition> functions;
    protected Map<String, IASTclassDefinition> clazzes;
    protected IASTexpression expression;
    
    public Map<String, IASTfunctionDefinition> getFunctionDefinitions() {
        return functions;
    }
    public Map<String, IASTclassDefinition> getClassDefinitions() {
        return clazzes;
    }
    public IASTexpression getBody() {
        return this.expression;
    }
}
