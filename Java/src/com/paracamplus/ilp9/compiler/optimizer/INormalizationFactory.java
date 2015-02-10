package com.paracamplus.ilp9.compiler.optimizer;

import java.util.Map;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.ast.IASTCprogram;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalVariable;
import com.paracamplus.ilp9.compiler.ast.IASTLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.IASTLocalVariable;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public interface INormalizationFactory {

    IASTCprogram newProgram(IASTfunctionDefinition[] functions,
                            Map<String, IASTclassDefinition> clazzes, 
                            IASTexpression expression);

    // Non uniform value types:
    
    public IASTvariable newVariable(String name) throws CompilationException;
    public IASTGlobalVariable newGlobalVariable(String name);
    public IASTLocalVariable newLocalVariable(String name);
    public IASTLocalFunctionVariable newLocalFunctionVariable(String name);
    public IASTGlobalFunctionVariable newGlobalFunctionVariable(String name);

    public IASToperator newOperator(String name);

    public IASTbinding newBinding(IASTvariable variable, 
                                  IASTexpression initialisation);

    public IASTfunctionDefinition newFunctionDefinition(
            String functionName,
            IASTvariable[] variables, 
            IASTexpression body);
    
    // Expression related:

    public IASTexpression newSequence(IASTexpression[] asts);

    public IASTexpression newAlternative(IASTexpression condition,
                                         IASTexpression consequence, 
                                         IASTexpression alternant);

    public IASTexpression newInvocation(
            IASTexpression function,
            IASTexpression[] arguments) throws CompilationException;
    public IASTexpression newComputedInvocation(
            IASTexpression function,
            IASTexpression[] arguments);
    public IASTexpression newPrimitiveInvocation(
            IASTvariable function,
            IASTexpression[] arguments);
    public IASTexpression newGlobalInvocation(
            IASTGlobalVariable funexpr,
            IASTexpression[] arguments);
    public IASTexpression newLocalFunctionInvocation(
            IASTLocalFunctionVariable function,
            IASTexpression[] arguments);

    public IASTexpression newUnaryOperation(IASToperator operator,
                                            IASTexpression operand);

    public IASTexpression newBinaryOperation(IASToperator operator,
            IASTexpression leftOperand, IASTexpression rightOperand);

    public IASTexpression newIntegerConstant(String value);

    public IASTexpression newFloatConstant(String value);

    public IASTexpression newStringConstant(String value);

    public IASTexpression newBooleanConstant(String value);

    public IASTexpression newAssignment(IASTvariable variable,
                                        IASTexpression value);

    public IASTexpression newBlock(IASTbinding[] binding,
                                   IASTexpression body);
    
    public IASTexpression newLoop(IASTexpression condition, 
                                  IASTexpression body);

    public IASTexpression newTry (IASTexpression body,
                                  IASTlambda catcher,
                                  IASTexpression finallyer );
    
    public IASTexpression newLambda (IASTvariable[] variables,
                                     IASTexpression body );
    
    public IASTexpression newCodefinitions (
            IASTfunctionDefinition[] functions,
            IASTexpression body );
    
    // Class related
    
    public IASTclassDefinition newClassDefinition(
            String className,
            String superClassName, 
            String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions);

    public IASTmethodDefinition newMethodDefinition(
            String methodName,
            IASTvariable[] variables, 
            IASTexpression body);

    public IASTexpression newInstantiation(String className,
                                           IASTexpression[] arguments);

    public IASTexpression newReadField(String fieldName, 
                                       IASTexpression object);

    public IASTexpression newWriteField(String fieldName,
                                        IASTexpression object, 
                                        IASTexpression value);

    public IASTexpression newSelf(IASTvariable variable);

    public IASTexpression newSend(String message, 
                                  IASTexpression receiver,
                                  IASTexpression[] arguments);
    
    public IASTexpression newSuper();
}
