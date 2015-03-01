/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTvisitor;

public class ASTsend implements IASTsend {

    public ASTsend (String messageName, 
                    IASTexpression receiver, 
                    IASTexpression[] arguments) {
        this.messageName = messageName;
        this.receiver = receiver;
        this.arguments = arguments;
    }
    private final String messageName;
    private final IASTexpression receiver;
    private final IASTexpression[] arguments;
    
    public String getMethodName() {
        return messageName;
    }

    public IASTexpression getReceiver() {
        return receiver;
    }

    public IASTexpression[] getArguments() {
        return arguments;
    }

    public <Result, Data, Anomaly extends Throwable> Result 
        accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
                throws Anomaly {
        return visitor.visit(this, data);
    }
}
