package com.paracamplus.ilp9.ast;

import java.util.Map;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTprogram;

public class ASTprogram extends AST implements IASTprogram {
    public ASTprogram(IASTfunctionDefinition[] functions,
                      Map<String, IASTclassDefinition> clazzes, 
                      IASTexpression expression) {
        this.functions = functions;
        this.clazzes = clazzes;
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
}
