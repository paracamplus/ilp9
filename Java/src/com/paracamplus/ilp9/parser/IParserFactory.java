package com.paracamplus.ilp9.parser;

import com.paracamplus.ilp9.interfaces.*;

public interface IParserFactory {
    IASTprogram newProgram(
    		IASTfunctionDefinition[] functions,
    		IASTclassDefinition[] clazzes,
            IASTexpression expression);
    
    IASTsequence newSequence(IASTexpression[] asts);

    IASTalternative newAlternative(
            IASTexpression condition,
            IASTexpression consequent,
            IASTexpression alternant);

    IASTvariable newVariable(String name);

    IASTreference newReference(IASTvariable variable);

    IASTinvocation newInvocation(
            IASTexpression function,
            IASTexpression[] arguments);

    IASTunaryOperation newUnaryOperation(
            String operatorName,
            IASTexpression operand);

    IASTbinaryOperation newBinaryOperation(
            String operatorName,
            IASTexpression leftOperand,
            IASTexpression rightOperand);

    IASTinteger newIntegerConstant(String value);

    IASTfloat newFloatConstant(String value);

    IASTstring newStringConstant(String value);

    IASTboolean newBooleanConstant(String value);

    IASTassignment newAssignment(
            IASTvariable variable,
            IASTexpression value);

    IASTblock newBlock(
            IASTvariable[] variables,
            IASTexpression[] initialisations,
            IASTexpression body);

    IASTloop newLoop(
            IASTexpression condition,
            IASTexpression body);

    IASTfunctionDefinition newFunctionDefinition(
            String functionName,
            IASTvariable[] variables,
            IASTexpression body);
    
    IASTclassDefinition newClassDefinition(
            String className,
            String superClassName,
            String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions );
    
    IASTmethodDefinition newMethodDefinition(
            String methodName,
            IASTvariable[] variables,
            IASTexpression body  );
    
    IASTinstantiation newInstantiation(
            String className,
            IASTexpression[] arguments );
    
    IASTreadField newReadField(
            String fieldName,
            IASTexpression object );
    
    IASTwriteField newWriteField(
            String fieldName,
            IASTexpression object,
            IASTexpression value );
    
    IASTself newSelf(IASTvariable variable);
    
    IASTsend newSend(
            String message,
            IASTexpression receiver,
            IASTexpression[] arguments );
    
    IASTsuper newSuper();
}
