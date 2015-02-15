package com.paracamplus.ilp9.compiler.normalizer;

import java.util.Map;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.interfaces.IASTCblock;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
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
    
     IASTCvariable newVariable(String name) throws CompilationException;
     IASTCglobalVariable newGlobalVariable(String name);
     IASTClocalVariable newLocalVariable(String name);
     IASTClocalFunctionVariable newLocalFunctionVariable(String name);
     IASTCglobalFunctionVariable newGlobalFunctionVariable(String name);

     IASToperator newOperator(String name);

     IASTfunctionDefinition newFunctionDefinition(
            IASTvariable functionVariable,
            IASTvariable[] variables,
            IASTexpression body);
    
    // Expression related:

     IASTexpression newSequence(IASTexpression[] asts);

     IASTexpression newAlternative(IASTexpression condition,
                                   IASTexpression consequence, 
                                   IASTexpression alternant);

     IASTexpression newInvocation(
            IASTexpression function,
            IASTexpression[] arguments) throws CompilationException;
     IASTexpression newComputedInvocation(
            IASTexpression function,
            IASTexpression[] arguments);
     IASTexpression newPrimitiveInvocation(
            IASTvariable function,
            IASTexpression[] arguments);
     IASTexpression newGlobalInvocation(
            IASTCglobalVariable funexpr,
            IASTexpression[] arguments);
     IASTexpression newLocalFunctionInvocation(
            IASTClocalFunctionVariable function,
            IASTexpression[] arguments);

     IASTexpression newUnaryOperation(IASToperator operator,
                                      IASTexpression operand);

     IASTexpression newBinaryOperation(IASToperator operator,
                                       IASTexpression leftOperand, 
                                       IASTexpression rightOperand);

     IASTexpression newIntegerConstant(String value);

     IASTexpression newFloatConstant(String value);

     IASTexpression newStringConstant(String value);

     IASTexpression newBooleanConstant(String value);

     IASTexpression newAssignment(IASTvariable variable,
                                  IASTexpression value);

     IASTCblock newBlock(IASTCblock.IASTCbinding[] binding,
                         IASTexpression body);

     IASTCblock.IASTCbinding newBinding(IASTvariable variable, 
                                        IASTexpression initialisation);

     IASTexpression newLoop(IASTexpression condition, 
                                  IASTexpression body);

     IASTexpression newTry (IASTexpression body,
                            IASTlambda catcher,
                            IASTexpression finallyer );
    
     IASTClambda newLambda (IASTvariable[] variables,
                            IASTexpression body );

    IASTCnamedLambda newNamedLambda(IASTvariable newFunctionVar,
                                    IASTvariable[] newvariables,
                                    IASTexpression newbody);
    
     IASTCcodefinitions newCodefinitions (
            IASTCnamedLambda[] functions,
            IASTexpression body );
    
    // Class related
    
     IASTclassDefinition newClassDefinition(
            String className,
            String superClassName, 
            String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions);

     IASTmethodDefinition newMethodDefinition(
            String methodName,
            IASTvariable[] variables, 
            IASTexpression body);

     IASTexpression newInstantiation(String className,
                                           IASTexpression[] arguments);

     IASTexpression newReadField(String fieldName, 
                                       IASTexpression object);

     IASTexpression newWriteField(String fieldName,
                                        IASTexpression object, 
                                        IASTexpression value);

     IASTexpression newSelf(IASTvariable variable);

     IASTexpression newSend(String message, 
                                  IASTexpression receiver,
                                  IASTexpression[] arguments);
    
     IASTexpression newSuper();
}
