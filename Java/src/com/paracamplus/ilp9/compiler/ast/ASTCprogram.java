/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.ast;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;

public class ASTCprogram extends com.paracamplus.ilp9.ast.ASTprogram 
implements IASTCprogram {

    public ASTCprogram (IASTCfunctionDefinition[] functions,
                        IASTCclassDefinition[] clazzes, 
                        IASTexpression expression) {
        super(functions, clazzes, expression);
        this.globalVariables = new HashSet<>();
        this.closureDefinitions = new Vector<>();
    }
    private Set<IASTCglobalVariable> globalVariables;
    private List<IASTClambda> closureDefinitions;
    
    public Set<IASTCglobalVariable> getGlobalVariables() {
        return globalVariables;
    }

    public void setGlobalVariables(Set<IASTCglobalVariable> gvs) {
        globalVariables = gvs;        
    }

    public void addClosureDefinition(IASTClambda f) {
        closureDefinitions.add(f);       
    }
    
    public List<IASTClambda> getClosureDefinitions () {
        return closureDefinitions;
    }
    
    public IASTCclassDefinition[] getClassDefinitions() {
        IASTclassDefinition[] cds = super.getClassDefinitions();
        IASTCclassDefinition[] newcds = new IASTCclassDefinition[cds.length];
        for ( int i=0 ; i<cds.length ; i++ ) {
            newcds[i] = (IASTCclassDefinition) cds[i];
        }
        return newcds;
    }
    
    public IASTCfunctionDefinition[] getFunctionDefinitions() {
        IASTfunctionDefinition[] fds = super.getFunctionDefinitions();
        IASTCfunctionDefinition[] newfds = 
                new IASTCfunctionDefinition[fds.length];
        for ( int i=0 ; i<fds.length ; i++ ) {
            newfds[i] = (IASTCfunctionDefinition) fds[i];
        }
        return newfds;
    }
}
