/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.normalizer;

import java.util.concurrent.atomic.AtomicInteger;

import com.paracamplus.ilp9.ast.ASTalternative;
import com.paracamplus.ilp9.ast.ASTassignment;
import com.paracamplus.ilp9.ast.ASTbinaryOperation;
import com.paracamplus.ilp9.ast.ASTboolean;
import com.paracamplus.ilp9.ast.ASTfloat;
import com.paracamplus.ilp9.ast.ASTinteger;
import com.paracamplus.ilp9.ast.ASTloop;
import com.paracamplus.ilp9.ast.ASToperator;
import com.paracamplus.ilp9.ast.ASTself;
import com.paracamplus.ilp9.ast.ASTsend;
import com.paracamplus.ilp9.ast.ASTsequence;
import com.paracamplus.ilp9.ast.ASTstring;
import com.paracamplus.ilp9.ast.ASTsuper;
import com.paracamplus.ilp9.ast.ASTtry;
import com.paracamplus.ilp9.ast.ASTunaryOperation;
import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.ast.ASTCblock;
import com.paracamplus.ilp9.compiler.ast.ASTCclassDefinition;
import com.paracamplus.ilp9.compiler.ast.ASTCcodefinitions;
import com.paracamplus.ilp9.compiler.ast.ASTCcomputedInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCfieldRead;
import com.paracamplus.ilp9.compiler.ast.ASTCfieldWrite;
import com.paracamplus.ilp9.compiler.ast.ASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.ast.ASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCglobalInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCglobalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCinstantiation;
import com.paracamplus.ilp9.compiler.ast.ASTClambda;
import com.paracamplus.ilp9.compiler.ast.ASTClocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTClocalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCmethodDefinition;
import com.paracamplus.ilp9.compiler.ast.ASTCnamedLambda;
import com.paracamplus.ilp9.compiler.ast.ASTCprimitiveInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCblock;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcodefinitions;
import com.paracamplus.ilp9.compiler.interfaces.IASTCcomputedInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldRead;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfieldWrite;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCinstantiation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCmethodDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTCnamedLambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprimitiveInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTself;
import com.paracamplus.ilp9.interfaces.IASTsend;
import com.paracamplus.ilp9.interfaces.IASTsuper;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class NormalizationFactory
implements INormalizationFactory {
    
    public NormalizationFactory() {
        this.count = new AtomicInteger(0);
    }
    protected AtomicInteger count;
    
    public IASTCprogram newProgram(
            IASTCfunctionDefinition[] functions,
            IASTCclassDefinition[] clazzes, 
            IASTexpression expression) {
        return new ASTCprogram(functions, clazzes, expression); 
    }

    // Various types of variables:
    
    public IASTCvariable newVariable(String name) throws CompilationException {
        throw new CompilationException("Uncategorized variable " + name);
    }
    public IASTClocalFunctionVariable newLocalFunctionVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTClocalFunctionVariable(newName);
    }
    public IASTClocalVariable newLocalVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTClocalVariable(newName);
    }
    public IASTCglobalVariable newGlobalVariable(String name) {
        return new ASTCglobalVariable(name);
    }
    public IASTCglobalFunctionVariable newGlobalFunctionVariable(String name) {
        return new ASTCglobalFunctionVariable(name);
    }
    public String newGlobalClosureName() {
        String newName = "ilpclosure" + count.incrementAndGet();
        return newName;
    }
    public IASTCglobalFunctionVariable newMethodVariable(String methodName) {
        String newName = methodName + "_" + count.incrementAndGet();
        return new ASTCglobalFunctionVariable(newName);
    }
    
    public IASToperator newOperator(String name) {
        return new ASToperator(name);
    }
        
    public IASTCfunctionDefinition newFunctionDefinition(
            IASTvariable functionVariable,
            IASTvariable[] variables, 
            IASTexpression body) {
       return new ASTCfunctionDefinition(functionVariable, variables, body);
    }
    
    public IASTexpression newSequence(IASTexpression[] asts) {
        return new ASTsequence(asts);
    }
    
    public IASTexpression newAlternative(
            IASTexpression condition,
            IASTexpression consequence, 
            IASTexpression alternant) {
        return new ASTalternative(condition, consequence, alternant);
    }
    
    // various types of invocation
    
    public IASTexpression newInvocation(
            IASTexpression function,
            IASTexpression[] arguments) throws CompilationException {
        throw new CompilationException("Uncategorized invocation ");
    }
    public IASTCcomputedInvocation newComputedInvocation(
            IASTexpression function,
            IASTexpression[] arguments) {
        return new ASTCcomputedInvocation(function, arguments);
    }
    public IASTCprimitiveInvocation newPrimitiveInvocation(
            IASTvariable function,
            IASTexpression[] arguments) {
        return new ASTCprimitiveInvocation(function, arguments);
    }
    public IASTCglobalInvocation newGlobalInvocation(
            IASTCglobalVariable function,
            IASTexpression[] arguments) {
        return new ASTCglobalInvocation(function, arguments);
    }
    public IASTClocalFunctionInvocation newLocalFunctionInvocation(
            IASTClocalFunctionVariable function,
            IASTexpression[] arguments) {
        return new ASTClocalFunctionInvocation(function, arguments);
    }
    
    public IASTexpression newUnaryOperation(
            IASToperator operator,
            IASTexpression operand) {
        return new ASTunaryOperation(operator, operand);
    }
    
    public IASTexpression newBinaryOperation(
            IASToperator operator,
            IASTexpression leftOperand, 
            IASTexpression rightOperand) {
        return new ASTbinaryOperation(operator, leftOperand, rightOperand);
    }
    
    public IASTexpression newIntegerConstant(String value) {
        return new ASTinteger(value);
    }
    
    public IASTexpression newFloatConstant(String value) {
        return new ASTfloat(value);
    }
    
    public IASTexpression newStringConstant(String value) {
        return new ASTstring(value);
    }
    
    public IASTexpression newBooleanConstant(String value) {
        return new ASTboolean(value);
    }
    
    public IASTexpression newAssignment(IASTvariable variable,
                                        IASTexpression value) {
        return new ASTassignment(variable, value);
    }
    
    public IASTCblock newBlock(IASTCblock.IASTCbinding[] binding, IASTexpression body) {
        return new ASTCblock(binding, body);
    }
    public IASTCblock.IASTCbinding newBinding(IASTvariable variable, IASTexpression initialisation) {
        return new ASTCblock.ASTCbinding(variable, initialisation);
    }
    
    public IASTexpression newLoop(IASTexpression condition, IASTexpression body) {
        return new ASTloop(condition, body);
    }
    
    public IASTexpression newTry(
            IASTexpression body, 
            IASTlambda catcher,
            IASTexpression finallyer) {
        return new ASTtry(body, catcher, finallyer);
    }
    
    public IASTClambda newLambda(IASTvariable[] variables,
                                 IASTexpression body) {
        String closureName = newGlobalClosureName();
        return new ASTClambda(closureName, variables, body);
    }
    
    public IASTCnamedLambda newNamedLambda(
            IASTvariable functionVariable,
            IASTvariable[] variables, 
            IASTexpression body) {
        return new ASTCnamedLambda(functionVariable, variables, body);
    }
    
    public IASTCcodefinitions newCodefinitions(
            IASTCnamedLambda[] functions,
            IASTexpression body) {
        return new ASTCcodefinitions(functions, body);
    }

    // Class related

    public IASTCclassDefinition newClassDefinition(
            String className,
            IASTCclassDefinition superClass, 
            String[] fieldNames,
            IASTCmethodDefinition[] methodDefinitions) {
        return new ASTCclassDefinition(
                className,
                superClass,
                fieldNames,
                methodDefinitions );
    }

    public IASTCmethodDefinition newMethodDefinition(
            IASTvariable methodVariable,
            IASTvariable[] variables, 
            IASTexpression body,
            String methodName,
            IASTCclassDefinition definingClass ) {
        return new ASTCmethodDefinition(
                methodVariable, variables, body, methodName, definingClass);
    }

    public IASTCinstantiation newInstantiation(
            IASTCclassDefinition clazz,
            IASTexpression[] arguments) {
        return new ASTCinstantiation(clazz, arguments);
    }

    public IASTCfieldRead newReadField(
            IASTCclassDefinition clazz,
            String fieldName, 
            IASTexpression target) {
        return new ASTCfieldRead(clazz, fieldName, target);
    }

    public IASTCfieldWrite newWriteField(
            IASTCclassDefinition clazz,
            String fieldName,
            IASTexpression target, 
            IASTexpression value) {
        return new ASTCfieldWrite(clazz, fieldName, target, value);
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
