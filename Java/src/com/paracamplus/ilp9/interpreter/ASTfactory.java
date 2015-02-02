package com.paracamplus.ilp9.interpreter;

import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinstantiation;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTreadField;
import com.paracamplus.ilp9.interfaces.IASTreference;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTwriteField;
import com.paracamplus.ilp9.parser.IParserFactory;

public class ASTfactory implements IParserFactory {

    public IASTprogram newProgram(IASTfunctionDefinition[] functions,
                                  IASTclassDefinition[] clazzes, 
                                  IASTexpression expression) {
        return new ASTprogram(functions, clazzes, expression);
    }

    public IASTsequence newSequence(IASTexpression[] asts) {
        return new ASTsequence(asts);
    }

    public IASTalternative newAlternative(IASTexpression condition,
                                          IASTexpression consequence, 
                                          IASTexpression alternant) {
        return new ASTalternative(condition, consequence, alternant);
    }

    public IASTvariable newVariable(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTreference newReference(IASTvariable variable) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTinvocation newInvocation(IASTexpression function,
            IASTexpression[] arguments) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTunaryOperation newUnaryOperation(String operatorName,
            IASTexpression operand) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTbinaryOperation newBinaryOperation(String operatorName,
            IASTexpression leftOperand, IASTexpression rightOperand) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTinteger newIntegerConstant(String value) {
        return new ASTinteger(value); 
    }

    public IASTfloat newFloatConstant(String value) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTstring newStringConstant(String value) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTboolean newBooleanConstant(String value) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTassignment newAssignment(IASTvariable variable,
            IASTexpression value) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTblock newBlock(IASTvariable[] variables,
            IASTexpression[] initialisations, IASTexpression body) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTloop newLoop(IASTexpression condition, IASTexpression body) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTfunctionDefinition newFunctionDefinition(String functionName,
            IASTvariable[] variables, IASTexpression body) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTclassDefinition newClassDefinition(String className,
            String superClassName, String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTmethodDefinition newMethodDefinition(String methodName,
            IASTvariable[] variables, IASTexpression body) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTinstantiation newInstantiation(String className,
            IASTexpression[] arguments) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTreadField newReadField(String fieldName, IASTexpression object) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTwriteField newWriteField(String fieldName,
            IASTexpression object, IASTexpression value) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTself newSelf(IASTvariable variable) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTsend newSend(String message, IASTexpression receiver,
            IASTexpression[] arguments) {
        // TODO Auto-generated method stub
        return null;
    }

    public IASTsuper newSuper() {
        // TODO Auto-generated method stub
        return null;
    }

}
