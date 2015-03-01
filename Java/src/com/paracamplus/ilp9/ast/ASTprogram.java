/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import java.util.Arrays;
import java.util.List;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTprogram;

public class ASTprogram extends AST implements IASTprogram {
    public ASTprogram(IASTfunctionDefinition[] functions,
                      IASTclassDefinition[] clazzes,
                      IASTexpression expression) {
        this.functions = Arrays.asList(functions);
        this.clazzes = clazzes;
        this.expression = expression;
    }
    protected List<IASTfunctionDefinition> functions;
    protected IASTclassDefinition[] clazzes;
    protected IASTexpression expression;
    
    public IASTfunctionDefinition[] getFunctionDefinitions() {
        return functions.toArray(new IASTfunctionDefinition[0]);
    }
    public IASTclassDefinition[] getClassDefinitions() {
        return clazzes;
    }
    public IASTexpression getBody() {
        return this.expression;
    }
}
