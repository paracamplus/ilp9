package com.paracamplus.ilp9.compiler.optimizer;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import com.paracamplus.ilp9.ast.ASTalternative;
import com.paracamplus.ilp9.ast.ASTassignment;
import com.paracamplus.ilp9.ast.ASTbinaryOperation;
import com.paracamplus.ilp9.ast.ASTblock;
import com.paracamplus.ilp9.ast.ASTboolean;
import com.paracamplus.ilp9.ast.ASTcodefinitions;
import com.paracamplus.ilp9.ast.ASTfloat;
import com.paracamplus.ilp9.ast.ASTinteger;
import com.paracamplus.ilp9.ast.ASTloop;
import com.paracamplus.ilp9.ast.ASToperator;
import com.paracamplus.ilp9.ast.ASTsequence;
import com.paracamplus.ilp9.ast.ASTstring;
import com.paracamplus.ilp9.ast.ASTtry;
import com.paracamplus.ilp9.ast.ASTunaryOperation;
import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.ast.ASTCComputedInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCGlobalInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCLocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCLocalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCPrimitiveInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.ast.ASTClambda;
import com.paracamplus.ilp9.compiler.ast.ASTCnamedLambda;
import com.paracamplus.ilp9.compiler.ast.ASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCComputedInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCPrimitiveInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCfunctionDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTvariable;

public class NormalizationFactory
implements INormalizationFactory {
    
    public NormalizationFactory() {
        this.count = new AtomicInteger(0);
    }
    protected AtomicInteger count;
    
    public IASTCprogram newProgram(
            IASTfunctionDefinition[] functions,
            Map<String, IASTclassDefinition> clazzes, 
            IASTexpression expression) {
        return new ASTCprogram(functions, clazzes, expression); 
    }

    // Various types of variables:
    
    public IASTCvariable newVariable(String name) throws CompilationException {
        throw new CompilationException("Uncategorized variable " + name);
    }
    public IASTCLocalFunctionVariable newLocalFunctionVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTCLocalFunctionVariable(newName);
    }
    public IASTCLocalVariable newLocalVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTCLocalVariable(newName);
    }
    public IASTCGlobalVariable newGlobalVariable(String name) {
        String newName = name;
        return new ASTCGlobalVariable(newName);
    }
    public IASTCGlobalFunctionVariable newGlobalFunctionVariable(String name) {
        String newName = name;
        return new ASTCGlobalFunctionVariable(newName);
    }
    public String newGlobalClosureName() {
        String newName = "ilpclosure" + count.incrementAndGet();
        return newName;
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
    public IASTCComputedInvocation newComputedInvocation(
            IASTexpression function,
            IASTexpression[] arguments) {
        return new ASTCComputedInvocation(function, arguments);
    }
    public IASTCPrimitiveInvocation newPrimitiveInvocation(
            IASTvariable function,
            IASTexpression[] arguments) {
        return new ASTCPrimitiveInvocation(function, arguments);
    }
    public IASTCGlobalInvocation newGlobalInvocation(
            IASTCGlobalVariable function,
            IASTexpression[] arguments) {
        return new ASTCGlobalInvocation(function, arguments);
    }
    public IASTCLocalFunctionInvocation newLocalFunctionInvocation(
            IASTCLocalFunctionVariable function,
            IASTexpression[] arguments) {
        return new ASTCLocalFunctionInvocation(function, arguments);
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
    
    public IASTexpression newBlock(IASTbinding[] binding, IASTexpression body) {
        return new ASTblock(binding, body);
    }
    public IASTbinding newBinding(IASTvariable variable, IASTexpression initialisation) {
        return new ASTblock.ASTbinding(variable, initialisation);
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
    
    public IASTnamedLambda newNamedLambda(
            IASTvariable functionVariable,
            IASTvariable[] variables, 
            IASTexpression body) {
        return new ASTCnamedLambda(functionVariable, variables, body);
    }
    
    public IASTexpression newCodefinitions(
            IASTnamedLambda[] functions,
            IASTexpression body) {
        return new ASTcodefinitions(functions, body);
    }

    // Class related
    
    public IASTclassDefinition newClassDefinition(String className,
            String superClassName, String[] fieldNames,
            IASTmethodDefinition[] methodDefinitions) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTmethodDefinition newMethodDefinition(String methodName,
            IASTvariable[] variables, IASTexpression body) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newInstantiation(String className,
            IASTexpression[] arguments) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newReadField(String fieldName, IASTexpression object) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newWriteField(String fieldName,
            IASTexpression object, IASTexpression value) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newSelf(IASTvariable variable) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newSend(String message, IASTexpression receiver,
            IASTexpression[] arguments) {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public IASTexpression newSuper() {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
}
