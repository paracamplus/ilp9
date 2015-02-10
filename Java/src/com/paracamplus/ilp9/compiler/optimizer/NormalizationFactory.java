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
import com.paracamplus.ilp9.ast.ASTfunctionDefinition;
import com.paracamplus.ilp9.ast.ASTinteger;
import com.paracamplus.ilp9.ast.ASTlambda;
import com.paracamplus.ilp9.ast.ASTloop;
import com.paracamplus.ilp9.ast.ASToperator;
import com.paracamplus.ilp9.ast.ASTsequence;
import com.paracamplus.ilp9.ast.ASTstring;
import com.paracamplus.ilp9.ast.ASTtry;
import com.paracamplus.ilp9.ast.ASTunaryOperation;
import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.ast.ASTComputedInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTGlobalInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTGlobalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTLocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.ASTLocalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTPrimitiveInvocation;
import com.paracamplus.ilp9.compiler.ast.ASTCprogram;
import com.paracamplus.ilp9.compiler.ast.IASTComputedInvocation;
import com.paracamplus.ilp9.compiler.ast.IASTCprogram;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalInvocation;
import com.paracamplus.ilp9.compiler.ast.IASTGlobalVariable;
import com.paracamplus.ilp9.compiler.ast.IASTLocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.ast.IASTLocalFunctionVariable;
import com.paracamplus.ilp9.compiler.ast.IASTLocalVariable;
import com.paracamplus.ilp9.compiler.ast.IASTPrimitiveInvocation;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
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
    
    public IASTvariable newVariable(String name) throws CompilationException {
        throw new CompilationException("Uncategorized variable " + name);
    }
    public IASTLocalFunctionVariable newLocalFunctionVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTLocalFunctionVariable(newName);
    }
    public IASTLocalVariable newLocalVariable(String name) {
        String newName = name + count.incrementAndGet();
        return new ASTLocalVariable(newName);
    }
    public IASTGlobalVariable newGlobalVariable(String name) {
        String newName = name;
        return new ASTGlobalVariable(newName);
    }
    public IASTGlobalFunctionVariable newGlobalFunctionVariable(String name) {
        String newName = name;
        return new ASTGlobalFunctionVariable(newName);
    }
    
    public IASToperator newOperator(String name) {
        return new ASToperator(name);
    }
        
    public IASTfunctionDefinition newFunctionDefinition(
            String functionName,
            IASTvariable[] variables, 
            IASTexpression body) {
       return new ASTfunctionDefinition(functionName, variables, body);
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
    public IASTComputedInvocation newComputedInvocation(
            IASTexpression function,
            IASTexpression[] arguments) {
        return new ASTComputedInvocation(function, arguments);
    }
    public IASTPrimitiveInvocation newPrimitiveInvocation(
            IASTvariable function,
            IASTexpression[] arguments) {
        return new ASTPrimitiveInvocation(function, arguments);
    }
    public IASTGlobalInvocation newGlobalInvocation(
            IASTGlobalVariable function,
            IASTexpression[] arguments) {
        return new ASTGlobalInvocation(function, arguments);
    }
    public IASTLocalFunctionInvocation newLocalFunctionInvocation(
            IASTLocalFunctionVariable function,
            IASTexpression[] arguments) {
        return new ASTLocalFunctionInvocation(function, arguments);
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
    
    public IASTexpression newLambda(IASTvariable[] variables,
                                    IASTexpression body) {
        return new ASTlambda(variables, body);
    }
    
    public IASTexpression newCodefinitions(
            IASTfunctionDefinition[] functions,
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
