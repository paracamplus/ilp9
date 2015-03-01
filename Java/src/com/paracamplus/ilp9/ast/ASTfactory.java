/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTcodefinitions;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfieldRead;
import com.paracamplus.ilp9.interfaces.IASTfieldWrite;
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinstantiation;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.parser.IParserFactory;

public class ASTfactory implements IParserFactory {

    public IASTprogram newProgram(IASTfunctionDefinition[] functions,
                                  IASTclassDefinition[] clazzes, 
                                  IASTexpression expression) {
        return new ASTprogram(functions, clazzes, expression);
    }
    
    public IASToperator newOperator(String name) {
        return new ASToperator(name);
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
        return new ASTvariable(name);
    }

    public IASTinvocation newInvocation(IASTexpression function,
                                        IASTexpression[] arguments) {
        return new ASTinvocation(function, arguments);
    }

    public IASTunaryOperation newUnaryOperation(IASToperator operator,
                                                IASTexpression operand) {
        return new ASTunaryOperation(operator, operand);
    }

    public IASTbinaryOperation newBinaryOperation(IASToperator operator,
            IASTexpression leftOperand, IASTexpression rightOperand) {
        return new ASTbinaryOperation(operator, leftOperand, rightOperand);
    }

    public IASTinteger newIntegerConstant(String value) {
        return new ASTinteger(value); 
    }

    public IASTfloat newFloatConstant(String value) {
        return new ASTfloat(value);
    }

    public IASTstring newStringConstant(String value) {
        return new ASTstring(value);
    }

    public IASTboolean newBooleanConstant(String value) {
        return new ASTboolean(value);
    }

    public IASTassignment newAssignment(IASTvariable variable,
                                        IASTexpression value) {
        return new ASTassignment(variable, value);
    }

    public IASTblock newBlock(IASTbinding[] binding,
                              IASTexpression body) {
        return new ASTblock(binding, body);
    }
    public IASTbinding newBinding(IASTvariable variable, IASTexpression initialisation) {
        return new ASTblock.ASTbinding(variable, initialisation);
    }

    public IASTloop newLoop(IASTexpression condition, IASTexpression body) {
        return new ASTloop(condition, body);
    }

    public IASTfunctionDefinition newFunctionDefinition(
            IASTvariable functionVariable,
            IASTvariable[] variables, 
            IASTexpression body) {
        return new ASTfunctionDefinition(functionVariable, variables, body);
    }

    public IASTtry newTry (IASTexpression body,
                           IASTlambda catcher,
                           IASTexpression finallyer ) {
        return new ASTtry(body, catcher, finallyer);
    }
    
    public IASTlambda newLambda (IASTvariable[] variables,
                                 IASTexpression body ) {
        return new ASTlambda(variables, body);
    }
    
    public IASTnamedLambda newNamedLambda (
            IASTvariable functionVariable,
            IASTvariable[] variables,
            IASTexpression body ) {
        return new ASTnamedLambda(functionVariable, variables, body);
    }
    
    public IASTcodefinitions newCodefinitions (
            IASTnamedLambda[] functions,
            IASTexpression body ) {
        return new ASTcodefinitions(functions, body);
    }
    
    public IASTclassDefinition newClassDefinition(
            String className,
            String superClassName, 
            String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions) {
        return new ASTclassDefinition(
                className,
                superClassName,
                fieldNames,
                methodDefinitions );
    }

    public IASTmethodDefinition newMethodDefinition(
            IASTvariable methodVariable,
            IASTvariable[] variables, 
            IASTexpression body,
            String methodName,
            String definingClassName ) {
        return new ASTmethodDefinition(
                methodVariable, variables, body, 
                methodName, definingClassName);
    }

    public IASTinstantiation newInstantiation(
            String className,
            IASTexpression[] arguments) {
        return new ASTinstantiation(className, arguments);
    }

    public IASTfieldRead newReadField(String fieldName, IASTexpression target) {
        return new ASTfieldRead(fieldName, target);
    }

    public IASTfieldWrite newWriteField(
            String fieldName,
            IASTexpression target, 
            IASTexpression value) {
        return new ASTfieldWrite(fieldName, target, value);
    }

    public IASTsend newSend(
            String message, 
            IASTexpression receiver,
            IASTexpression[] arguments) {
        return new ASTsend(message, receiver, arguments);
    }

    public IASTself newSelf() {
        return new ASTself();
    }

    public IASTsuper newSuper() {
        return new ASTsuper();
    }
}
