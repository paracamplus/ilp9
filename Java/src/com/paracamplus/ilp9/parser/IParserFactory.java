package com.paracamplus.ilp9.parser;

import java.util.Map;

import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTcodefinitions;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinstantiation;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTreadField;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTwriteField;

public interface IParserFactory {
    IASTprogram newProgram(
    		IASTfunctionDefinition[] functions,
    		Map<String, IASTclassDefinition> clazzes,
            IASTexpression expression);
    
    IASTsequence newSequence(IASTexpression[] asts);

    IASTalternative newAlternative(
            IASTexpression condition,
            IASTexpression consequence,
            IASTexpression alternant);

    IASToperator newOperator(String name);
    
    IASTvariable newVariable(String name);
    
    IASTinvocation newInvocation(
            IASTexpression function,
            IASTexpression[] arguments);

    IASTunaryOperation newUnaryOperation(
            IASToperator operator,
            IASTexpression operand);

    IASTbinaryOperation newBinaryOperation(
            IASToperator operator,
            IASTexpression leftOperand,
            IASTexpression rightOperand);

    IASTinteger newIntegerConstant(String value);

    IASTfloat newFloatConstant(String value);

    IASTstring newStringConstant(String value);

    IASTboolean newBooleanConstant(String value);

    IASTassignment newAssignment(IASTvariable variable,
                                 IASTexpression value);

    IASTblock newBlock(IASTbinding[] binding,
                       IASTexpression body);

    IASTbinding newBinding(IASTvariable v, IASTexpression exp);
    
    IASTloop newLoop(IASTexpression condition,
                     IASTexpression body);

    IASTfunctionDefinition newFunctionDefinition(
            String functionName,
            IASTvariable[] variables,
            IASTexpression body);
    
    IASTtry newTry (IASTexpression body,
                    IASTlambda catcher,
                    IASTexpression finallyer );

    IASTlambda newLambda (IASTvariable[] variables,
                          IASTexpression body );

    IASTcodefinitions newCodefinitions(IASTfunctionDefinition[] functions,
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
