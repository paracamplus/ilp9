/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.normalizer;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.interfaces.IASTCblock;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCmethodDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTvariable;

 public interface INormalizationFactory {

    IASTCprogram newProgram(IASTCfunctionDefinition[] functions,
                            IASTCclassDefinition[] clazzes, 
                            IASTexpression expression);

    // Non uniform value types:
    
     IASTCvariable newVariable(String name) throws CompilationException;
     IASTCglobalVariable newGlobalVariable(String name);
     IASTClocalVariable newLocalVariable(String name);
     IASTClocalFunctionVariable newLocalFunctionVariable(String name);
     IASTCglobalFunctionVariable newGlobalFunctionVariable(String name);
     IASTCglobalFunctionVariable newMethodVariable(String name);
     
     IASToperator newOperator(String name);

     IASTCfunctionDefinition newFunctionDefinition(
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
    
     IASTCclassDefinition newClassDefinition(
            String className,
            IASTCclassDefinition superClass, 
            String[] fieldNames,
            IASTCmethodDefinition[] methodDefinitions);

     IASTCmethodDefinition newMethodDefinition(
             IASTvariable methodVariable,
             IASTvariable[] variables, 
             IASTexpression body,
             String methodName,
             IASTCclassDefinition definingClass );
     
     IASTexpression newInstantiation(IASTCclassDefinition clazz,
                                     IASTexpression[] arguments);

     IASTexpression newReadField(IASTCclassDefinition clazz,
                                 String fieldName, 
                                 IASTexpression target);

     IASTexpression newWriteField(IASTCclassDefinition clazz,
                                  String fieldName,
                                  IASTexpression target, 
                                  IASTexpression value);

     IASTexpression newSelf();

     IASTexpression newSend(String message, 
                            IASTexpression receiver,
                            IASTexpression[] arguments);
    
     IASTexpression newSuper();
}
