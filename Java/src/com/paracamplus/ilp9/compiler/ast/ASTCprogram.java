package com.paracamplus.ilp9.compiler.ast;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;

public class ASTCprogram extends com.paracamplus.ilp9.ast.ASTprogram 
implements IASTCprogram {

    public ASTCprogram (IASTfunctionDefinition[] functions,
                       Map<String, IASTclassDefinition> clazzes, 
                       IASTexpression expression) {
        super(functions, clazzes, expression);
        this.globalVariables = new HashSet<>();
        this.closureDefinitions = new Vector<>();
    }
    private Set<IASTCGlobalVariable> globalVariables;
    private List<IASTClambda> closureDefinitions;
    
    public Set<IASTCGlobalVariable> getGlobalVariables() {
        return globalVariables;
    }

    public void setGlobalVariables(Set<IASTCGlobalVariable> gvs) {
        globalVariables = gvs;        
    }

    public void addClosureDefinition(IASTClambda f) {
        closureDefinitions.add(f);       
    }
    
    public List<IASTClambda> getClosureDefinitions () {
        return closureDefinitions;
    }
}
