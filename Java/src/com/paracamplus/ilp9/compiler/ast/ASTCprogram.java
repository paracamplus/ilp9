package com.paracamplus.ilp9.compiler.ast;

import java.util.List;
import java.util.Map;
import java.util.Vector;

import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;

public class ASTCprogram extends com.paracamplus.ilp9.ast.ASTprogram 
implements IASTCprogram {

    public ASTCprogram (IASTfunctionDefinition[] functions,
                       Map<String, IASTclassDefinition> clazzes, 
                       IASTexpression expression) {
        super(functions, clazzes, expression);
        this.globalVariables = new Vector<>();
    }
    private List<IASTGlobalVariable> globalVariables;
    
    public List<IASTGlobalVariable> getGlobalVariables() {
        return globalVariables;
    }

    public void setGlobalVariables(List<IASTGlobalVariable> gvs) {
        globalVariables = gvs;        
    }
}
