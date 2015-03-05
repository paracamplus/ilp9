/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.parser;

import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface IParserFactory {
    IASTprogram newProgram(
    		IASTfunctionDefinition[] functions,
    		IASTclassDefinition[] clazzes,
            IASTexpression expression);
    
    IASTexpression newSequence(IASTexpression[] asts);

    IASTexpression newAlternative(
            IASTexpression condition,
            IASTexpression consequence,
            IASTexpression alternant);

    IASToperator newOperator(String name);
    
    IASTvariable newVariable(String name);
    
    IASTexpression newInvocation(
            IASTexpression function,
            IASTexpression[] arguments);

    IASTexpression newUnaryOperation(
            IASToperator operator,
            IASTexpression operand);

    IASTexpression newBinaryOperation(
            IASToperator operator,
            IASTexpression leftOperand,
            IASTexpression rightOperand);

    IASTexpression newIntegerConstant(String value);

    IASTexpression newFloatConstant(String value);

    IASTexpression newStringConstant(String value);

    IASTexpression newBooleanConstant(String value);

    IASTexpression newAssignment(IASTvariable variable,
                                 IASTexpression value);

    IASTexpression newBlock(IASTbinding[] binding,
                            IASTexpression body);

    IASTbinding newBinding(IASTvariable v, IASTexpression exp);
    
    IASTexpression newLoop(IASTexpression condition,
                           IASTexpression body);

    IASTfunctionDefinition newFunctionDefinition(
            IASTvariable functionVariable,
            IASTvariable[] variables,
            IASTexpression body);
    
    IASTexpression newTry (IASTexpression body,
                           IASTlambda catcher,
                           IASTexpression finallyer );

    IASTlambda newLambda (IASTvariable[] variables,
                              IASTexpression body );

    IASTnamedLambda newNamedLambda(
            IASTvariable functionVariable,
            IASTvariable[] variables,
            IASTexpression body );
    
    IASTexpression newCodefinitions(IASTnamedLambda[] functions,
                                    IASTexpression body);

    IASTclassDefinition newClassDefinition(
            String className,
            String superClassName,
            String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions );
    
    IASTmethodDefinition newMethodDefinition(
            IASTvariable methodVariable,
            IASTvariable[] variables,
            IASTexpression body, 
            String methodName,
            String definingClassName  );
    
    IASTexpression newInstantiation(
            String className,
            IASTexpression[] arguments );
    
    IASTexpression newReadField(
            String fieldName,
            IASTexpression object );
    
    IASTexpression newWriteField(
            String fieldName,
            IASTexpression object,
            IASTexpression value );
    
    IASTvariable newSelf();
    
    IASTexpression newSend(
            String message,
            IASTexpression receiver,
            IASTexpression[] arguments );
    
    IASTexpression newSuper();
}
