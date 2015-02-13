package com.paracamplus.ilp9.compiler;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import com.paracamplus.ilp9.ast.ASTboolean;
import com.paracamplus.ilp9.ast.ASTvariable;
import com.paracamplus.ilp9.compiler.ast.ASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.ast.ASTCLocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCComputedInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalFunctionVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCGlobalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalFunctionInvocation;
import com.paracamplus.ilp9.compiler.interfaces.IASTCLocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTClambda;
import com.paracamplus.ilp9.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp9.compiler.optimizer.INormalizationFactory;
import com.paracamplus.ilp9.compiler.optimizer.NormalizationFactory;
import com.paracamplus.ilp9.compiler.optimizer.Normalizer;
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
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
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
import com.paracamplus.ilp9.interfaces.IASTvisitor;
import com.paracamplus.ilp9.interfaces.IASTwriteField;

public class Compiler implements
        IASTvisitor<Void, Compiler.Context, CompilationException> {
    
    public static class Context {
        public Context (IDestination destination) {
            this.destination = destination;
        }
        public IDestination destination;
        public static AtomicInteger counter = new AtomicInteger(0);
        
        public IASTvariable newTemporaryVariable () {
            int i = counter.incrementAndGet();
            return new ASTvariable("ILPtmp" + i);
        }
        
        public Context redirect (IDestination d) {
            if ( d == destination ) {
                return this;
            } else {
                return new Context(d);
            }
        }
    }
    
    // 
    
    public Compiler (IOperatorEnvironment ioe,
                     IGlobalVariableEnvironment igve ) {
        this.operatorEnvironment = ioe;
        this.globalVariableEnvironment = igve;
    }
    private final IOperatorEnvironment operatorEnvironment;
    private final IGlobalVariableEnvironment globalVariableEnvironment;
    
    public void setOptimizer (IOptimizer optimizer) {
        this.optimizer = optimizer;
    }
    private IOptimizer optimizer;
    
    //

    public void emit (String s) throws CompilationException {
        try {
            out.append(s);
        } catch (IOException e) {
            throw new CompilationException(e);
        }
    }
    public void emit (char c) throws CompilationException {
        try {
            out.append(c);
        } catch (IOException e) {
            throw new CompilationException(e);
        }
    }
    public void emit (int i) throws CompilationException {
        try {
            out.append(Integer.toString(i));
        } catch (IOException e) {
            throw new CompilationException(e);
        }
    }
    
    public IASTCprogram normalize(IASTprogram program) 
            throws CompilationException {
        INormalizationFactory nf = new NormalizationFactory();
        IASTCprogram cprogram = nf.newProgram(
                program.getFunctionDefinitions(),
                program.getClassDefinitions(),
                program.getBody());
        IOptimizer normalizer = new Normalizer(nf);
        IASTCprogram newprogram = normalizer.transform(cprogram);
        return newprogram;
    }
   
    public String compile(IASTprogram program) throws CompilationException {
        
        IASTCprogram newprogram = normalize(program);
        newprogram = optimizer.transform(newprogram);

        GlobalVariableCollector gvc = new GlobalVariableCollector();
        Set<IASTCGlobalVariable> gvs = gvc.analyze(newprogram);
        newprogram.setGlobalVariables(gvs);
        
        FreeVariableCollector fvc = new FreeVariableCollector(newprogram);
        newprogram = fvc.analyze();
      
        Context context = new Context(NoDestination.NO_DESTINATION);
        StringWriter sw = new StringWriter();
        try {
            out = new BufferedWriter(sw);
            visit(newprogram, context);
            out.flush();
        } catch (IOException exc) {
            throw new CompilationException(exc);
        }
        return sw.toString();
    }
    private Writer out;

    //
    
    public Void visit(IASTCprogram iast, Context context)
            throws CompilationException {
        emit(cProgramPrefix);
        
        emit(cGlobalVariablesPrefix);
        for ( IASTCGlobalVariable gv : iast.getGlobalVariables() ) {
            emit("ILP_Object ");
            emit(gv.getMangledName());
            emit(";\n");
        }
        emit(cGlobalVariablesSuffix);
        
        emit(cPrototypesPrefix);
        Context c = context.redirect(NoDestination.NO_DESTINATION);
        for ( IASTfunctionDefinition ifd : iast.getFunctionDefinitions() ) {
            this.emitPrototype(ifd, c);
        }
        for ( IASTClambda closure : iast.getClosureDefinitions() ) {
            this.emitPrototype(closure, c);
        }
        emit(cFunctionsPrefix);
        for ( IASTfunctionDefinition ifd : iast.getFunctionDefinitions() ) {
            this.visit(ifd, c);
        }
        for ( IASTClambda closure : iast.getClosureDefinitions() ) {
            this.emitFunction(closure, c);
        }
        emit(cFunctionsSuffix);
        
        emit(cBodyPrefix);
        Context cr = context.redirect(ReturnDestination.RETURN_DESTINATION);
        iast.getBody().accept(this, cr);
        emit(cBodySuffix);
        
        emit(cProgramSuffix);
        return null;
    }
    protected String cProgramPrefix = ""
            + "#include <stdio.h> \n"
            + "#include <stdlib.h> \n"
            + "#include \"ilp.h\" \n\n";
    protected String cGlobalVariablesPrefix = ""
            + "/* Global variables */ \n";
    protected String cGlobalVariablesSuffix = ""
            + "\n";
    protected String cPrototypesPrefix = ""
            + "/* Global prototypes */ \n";
    protected String cFunctionsPrefix = "\n"
            + "/* Global functions */ \n";
    protected String cFunctionsSuffix = "\n";
    protected String cBodyPrefix = "\n"
            + "ILP_Object ilp_program () \n"
            + "{ \n";
    protected String cBodySuffix = "\n"
            + "} \n";
    protected String cProgramSuffix = "\n"
            + "static ILP_Object ilp_caught_program () {\n"
            + "  struct ILP_catcher* current_catcher = ILP_current_catcher;\n"
            + "  struct ILP_catcher new_catcher;\n\n"
            + "  if ( 0 == setjmp(new_catcher._jmp_buf) ) {\n"
            + "    ILP_establish_catcher(&new_catcher);\n"
            + "    return ilp_program();\n"
            + "  };\n"
            + "  return ILP_current_exception;\n"
            + "}\n\n"
            + "int main (int argc, char *argv[]) \n"
            + "{ \n"
            + "  ILP_print(ilp_caught_program()); \n"
            + "  ILP_newline(); \n"
            + "  return EXIT_SUCCESS; \n"
            + "} \n";    
    
    public Void visit(IASTsequence iast, Context context)
            throws CompilationException {
        IASTvariable tmp = context.newTemporaryVariable();
        IASTexpression[] expressions = iast.getExpressions();
        Context c = context.redirect(new AssignDestination(tmp));
        emit("{ \n");
        emit("  ILP_Object " + tmp.getMangledName() + "; \n");
        for ( IASTexpression expr : expressions ) {
            expr.accept(this, c);
        }
        emit(context.destination.compile());
        emit(tmp.getMangledName());
        emit("; \n} \n");
        return null;
    }

    public Void visit(IASTvariable iast, Context context)
            throws CompilationException {
        if ( iast instanceof ASTCLocalVariable ) {
            return visit((ASTCLocalVariable) iast, context);
        } else {
            return visit((ASTCGlobalVariable) iast, context);
        }
    }
    
    public Void visit(ASTCLocalVariable iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        if ( iast.isClosed() ) {
            emit("ILP_Box2Value(");
            emit(iast.getMangledName());
            emit(")");
        } else {
            emit(iast.getMangledName());
        }
        emit("; \n");
        return null;
    }
    public Void visit(ASTCGlobalVariable iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        emit(globalVariableEnvironment.getCName(iast));
        emit("; \n");
        return null;
    }

    public Void visit(IASTboolean iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        if ( iast.getValue() ) {
            emit("ILP_TRUE");
        } else {
            emit("ILP_FALSE");
        }
        emit("; \n");
        return null;
    }

    public Void visit(IASTinteger iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        emit("ILP_Integer2ILP(");
        emit(iast.getValue().toString());
        emit("); \n");
        return null;
    }
    
    public Void visit(IASTfloat iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        emit("ILP_Float2ILP(");
        emit(iast.getValue().toString());
        emit("); \n");
        return null;
    }

    public Void visit(IASTstring iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        emit(" ILP_String2ILP(\"");
        final String s = iast.getValue();
        for ( int i=0 ; i<s.length() ; i++ ) {
          char c = s.charAt(i);
          switch ( c ) {
          case '\\':
          case '"': {
            emit("\\");
          }
        //$FALL-THROUGH$
        default: {
            emit(c);
          }
          }
        }
        emit("\"); \n");
        return null;
    }

    public Void visit(IASTunaryOperation iast, Context context)
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(tmp1));
        iast.getOperand().accept(this, c1);
        String cName = operatorEnvironment.getUnaryOperator(iast.getOperator());
        emit(context.destination.compile());
        emit(cName);
        emit("(");
        emit(tmp1.getMangledName());
        emit(");\n");
        emit("} \n");
        return null;
    }

    public Void visit(IASTbinaryOperation iast, Context context)
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        IASTvariable tmp2 = context.newTemporaryVariable();
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        emit("  ILP_Object " + tmp2.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(tmp1));
        iast.getLeftOperand().accept(this, c1);
        Context c2 = context.redirect(new AssignDestination(tmp2));
        iast.getRightOperand().accept(this, c2);
        String cName = operatorEnvironment.getBinaryOperator(iast.getOperator());
        emit(context.destination.compile());
        emit(cName);
        emit("(");
        emit(tmp1.getMangledName());
        emit(", ");
        emit(tmp2.getMangledName());
        emit(");\n");
        emit("} \n");
        return null;
    }
    
    public Void visit(IASToperator iast, Context context)
           throws CompilationException {
        throw new RuntimeException("Should never be called");
   }
   
    public Void visit(IASTalternative iast, Context context)
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c = context.redirect(new AssignDestination(tmp1));
        iast.getCondition().accept(this, c);
        emit("  if ( ILP_isEquivalentToTrue(");
        emit(tmp1.getMangledName());
        emit(" ) ) {\n");
        iast.getConsequence().accept(this, context);
        if ( iast.isTernary() ) {
            emit("\n  } else {\n");
            iast.getAlternant().accept(this, context);
        }
        emit("\n  }\n}\n");
        return null;
    }
    
    public Void visit(IASTassignment iast, Context context)
            throws CompilationException {
        if ( iast.getVariable() instanceof IASTCLocalVariable ) {
            return visitLocalAssignment(iast, context);
        } else {
            return visitNonLocalAssignment(iast, context);
        }
    }
    
    private Void visitLocalAssignment(IASTassignment iast, Context context) 
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(tmp1));
        iast.getExpression().accept(this, c1);
        // Cast ensured by visit(IASTassignment...)
        IASTCLocalVariable lv = (IASTCLocalVariable) iast.getVariable();
        emit(context.destination.compile());
        emit("(");
        if ( lv.isClosed() ) {
            emit("ILP_SetBoxedValue(");
            emit(lv.getMangledName());
            emit(", ");
            emit(tmp1.getMangledName());
            emit(")");
        } else {
            emit(lv.getMangledName());
            emit(" = ");
            emit(tmp1.getMangledName());
        }
        emit("); \n} \n");
        return null;
    }
    
    public Void visitNonLocalAssignment(IASTassignment iast, Context context) 
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(tmp1));
        iast.getExpression().accept(this, c1);
        emit(context.destination.compile());
        emit("(");
        emit(iast.getVariable().getMangledName());
        emit(" = ");
        emit(tmp1.getMangledName());
        emit("); \n} \n");
        return null;
    }
    
    public Void visit(IASTblock iast, Context context)
            throws CompilationException {
        emit("{ \n");
        IASTbinding[] bindings = iast.getBindings();
        IASTvariable[] tmps = new IASTvariable[bindings.length];
        for ( int i=0 ; i<bindings.length ; i++ ) {
            IASTvariable tmp = context.newTemporaryVariable();
            emit("  ILP_Object " + tmp.getMangledName() + "; \n");
            tmps[i] = tmp;
        }
        for ( int i=0 ; i<bindings.length ; i++ ) {
            IASTbinding binding = bindings[i];
            IASTvariable tmp = tmps[i];
            Context c = context.redirect(new AssignDestination(tmp));
            binding.getInitialisation().accept(this, c);
        }
        emit("\n  {\n");
        for ( int i=0 ; i<bindings.length ; i++ ) {
            IASTbinding binding = bindings[i];
            IASTvariable tmp = tmps[i];
            IASTvariable variable = binding.getVariable();
            emit("    ILP_Object ");
            emit(variable.getMangledName());
            emit(" = ");
            if ( variable instanceof IASTCLocalVariable ) {
                IASTCLocalVariable lv = (IASTCLocalVariable) variable;
                if ( lv.isClosed() ) {
                    emit("ILP_Value2Box(");
                    emit(tmp.getMangledName());
                    emit(")");
                } else {
                    emit(tmp.getMangledName());
                }
            } else {
                emit(tmp.getMangledName());
            }
            emit(";\n");
        }
        iast.getBody().accept(this, context);
        emit("\n  }\n}\n");
        return null;
    }
    
    public Void visit(IASTinvocation iast, Context context)
            throws CompilationException {
        if ( iast instanceof IASTCLocalFunctionInvocation ) {
            return visitGeneralInvocation(iast, context);
        } else if ( iast instanceof IASTCGlobalInvocation ) {
            return visit((IASTCGlobalInvocation) iast, context);
        } else if ( iast instanceof IASTCComputedInvocation ) {
            return visit((IASTCComputedInvocation) iast, context);
        } else {
            return visitGeneralInvocation(iast, context);
        }
    }
    
    public Void visit(IASTCGlobalInvocation iast, Context context)
            throws CompilationException {
        emit("{ \n");
        IASTexpression[] arguments = iast.getArguments();
        IASTvariable[] tmps = new IASTvariable[arguments.length];
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTvariable tmp = context.newTemporaryVariable();
            emit("  ILP_Object " + tmp.getMangledName() + "; \n");
            tmps[i] = tmp;
        }
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTexpression expression = arguments[i];
            IASTvariable tmp = tmps[i];
            Context c = context.redirect(new AssignDestination(tmp));
            expression.accept(this, c);
        }
        emit(context.destination.compile());
        if ( globalVariableEnvironment.isPrimitive(iast.getFunction()) ) {
            // check arity statically!
            IPrimitive fun = globalVariableEnvironment
                    .getPrimitiveDescription(iast.getFunction());
            emit(fun.getCName());
            emit("(");
        } else if (iast.getFunction() instanceof IASTCGlobalFunctionVariable) {
            // check arity statically!
            emit(iast.getFunction().getMangledName());
            emit("(");
        } else {
            emit("ILP_invoke(");
            emit(iast.getFunction().getMangledName());
            emit(", ");
            emit(arguments.length);
            if ( 0 < arguments.length ) {
                emit(", ");
            }
        }
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTvariable tmp = tmps[i];
            emit(tmp.getMangledName());
            if ( i < arguments.length-1 ) {
                emit(", ");
            }
        }
        emit(");\n}\n");
        return null;        
    }
     
    public Void visit(IASTCComputedInvocation iast, Context context)
        throws CompilationException {
        return visitGeneralInvocation(iast, context);
    }
    
    public Void visitGeneralInvocation(IASTinvocation iast, Context context)
            throws CompilationException {
        emit("{ \n");
        IASTexpression fexpr = iast.getFunction();
        IASTvariable tmpf = context.newTemporaryVariable();
        emit("  ILP_Object " + tmpf.getMangledName() + "; \n");
        IASTexpression[] arguments = iast.getArguments();
        IASTvariable[] tmps = new IASTvariable[arguments.length];
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTvariable tmp = context.newTemporaryVariable();
            emit("  ILP_Object " + tmp.getMangledName() + "; \n");
            tmps[i] = tmp;
        }
        Context cf = context.redirect(new AssignDestination(tmpf));
        fexpr.accept(this, cf);
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTexpression expression = arguments[i];
            IASTvariable tmp = tmps[i];
            Context c = context.redirect(new AssignDestination(tmp));
            expression.accept(this, c);
        }
        emit(context.destination.compile());
        emit("ILP_invoke(");
        emit(tmpf.getMangledName());
        emit(", ");
        emit(arguments.length);
        for ( int i=0 ; i<arguments.length ; i++ ) {
            IASTvariable tmp = tmps[i];
            emit(", ");
            emit(tmp.getMangledName());
        }
        emit(");\n}\n");
        return null;
    }
    
    public Void visit(IASTcodefinitions iast, Context context)
            throws CompilationException {
        emit("{ \n");
        IASTnamedLambda[] functions = iast.getFunctions();
        for ( IASTnamedLambda ifd : functions ) {
            emit("  ILP_Object ");
            emit(ifd.getFunctionVariable().getMangledName());
            emit(" = ILP_Value2Box(NULL); \n");
        }
        for ( IASTnamedLambda ifd : functions ) {
            IASTClambda fun = (IASTClambda) ifd;
            emit("ILP_SetBoxedValue(");
            emit(ifd.getFunctionVariable().getMangledName());
            emit(", ILP_make_closure(");
            emit(fun.getName());
            emit(", ");
            emit(fun.getVariables().length);
            emit(", ");
            emit(fun.getClosedVariables().size());
            for ( IASTvariable variable : fun.getClosedVariables() ) {
                emit(", ");
                emit(variable.getMangledName());
            }
            emit("));\n");
        }
        iast.getBody().accept(this, context);
        emit("\n} \n");
        return null;
    }

    public void emitPrototype(IASTfunctionDefinition iast, Context context)
            throws CompilationException {
        emit("ILP_Object ");
        emit(iast.getMangledName());
        emit("(\n");
        IASTvariable[] variables = iast.getVariables();
        for ( int i=0 ; i< variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            emit("    ILP_Object ");
            emit(variable.getMangledName());
            if ( i < variables.length-1 ) {
                emit(",\n");
            }
        }
        emit(");\n");
    }
    public Void visit(IASTfunctionDefinition iast, Context context)
            throws CompilationException {
        emit("ILP_Object ");
        emit(iast.getMangledName());
        emit("(\n");
        IASTvariable[] variables = iast.getVariables();
        for ( int i=0 ; i< variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            emit("    ILP_Object ");
            emit(variable.getMangledName());
            if ( i < variables.length-1 ) {
                emit(",\n");
            }
        }
        emit(") {\n");
        for ( IASTvariable variable : variables ) {
            try {
                // Cast ensured by normalizer:
                IASTCLocalVariable lv = (IASTCLocalVariable) variable;
                if ( lv.isClosed() ) {
                    emit(lv.getMangledName());
                    emit(" = ");
                    emit("ILP_Value2Box(");
                    emit(lv.getMangledName());
                    emit("); \n");
                }
            } catch (ClassCastException exc) {
                throw new RuntimeException("Should not occur");
            }
        }
        Context c = context.redirect(ReturnDestination.RETURN_DESTINATION);
        iast.getBody().accept(this, c);
        emit("}\n");
        return null;
    }
    
    public void emitPrototype(IASTClambda iast, Context context)
            throws CompilationException {
        emit("ILP_Object ");
        emit(iast.getMangledName());
        emit("(ILP_Closure ilp_closure");
        IASTvariable[] variables = iast.getVariables();
        for ( int i=0 ; i< variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            emit(",\n    ILP_Object ");
            emit(variable.getMangledName());
        }
        emit(");\n");
    }
    
    public void emitFunction(IASTClambda iast, Context context)
            throws CompilationException {
        emit("ILP_Object ");
        emit(iast.getMangledName());
        emit("(ILP_Closure ilp_closure");
        IASTvariable[] variables = iast.getVariables();
        for ( int i=0 ; i< variables.length ; i++ ) {
            IASTvariable variable = variables[i];
            emit(",\n    ILP_Object ");
            emit(variable.getMangledName());
        }
        emit(") {\n");
        int i = 0;
        for ( IASTvariable variable : iast.getClosedVariables() ) {
            emit("ILP_Object ");
            emit(variable.getMangledName());
            emit(" = ilp_closure->_content.asClosure.closed_variables[" + i++ + "]; \n");
        }
        for ( IASTvariable variable : variables ) {
            try {
                // Cast ensured by normalizer:
                IASTCLocalVariable lv = (IASTCLocalVariable) variable;
                if ( lv.isClosed() ) {
                    emit(lv.getMangledName());
                    emit(" = ");
                    emit("ILP_Value2Box(");
                    emit(lv.getMangledName());
                    emit("); \n");
                }
            } catch (ClassCastException exc) {
                throw new RuntimeException("Should not occur");
            }
        }
        Context c = context.redirect(ReturnDestination.RETURN_DESTINATION);
        iast.getBody().accept(this, c);
        emit("}\n");
    }
    
    public Void visit(IASTlambda iast, Context context)
            throws CompilationException {
        IASTClambda fun = (IASTClambda) iast;
        emit(context.destination.compile());
        emit("ILP_make_closure(");
        emit(fun.getMangledName());
        emit(", ");
        emit(fun.getVariables().length);
        emit(", ");
        emit(fun.getClosedVariables().size());
        for ( IASTvariable variable : fun.getClosedVariables() ) {
            emit(", ");
            emit(variable.getMangledName());
        }
        emit(");\n");
        return null;
    }
    
    public Void visit(IASTloop iast, Context context)
            throws CompilationException {
        emit("while ( 1 ) { \n");
        IASTvariable tmp = context.newTemporaryVariable();
        emit("  ILP_Object " + tmp.getMangledName() + "; \n");
        Context c = context.redirect(new AssignDestination(tmp));
        iast.getCondition().accept(this, c);
        emit("  if ( ILP_isEquivalentToTrue(");
        emit(tmp.getMangledName());
        emit(") ) {\n");
        Context cb = context.redirect(VoidDestination.VOID_DESTINATION);
        iast.getBody().accept(this, cb);
        emit("\n} else { \n");
        emit("    break; \n");
        emit("\n}\n}\n");
        whatever.accept(this, context);
        return null;
    }
    
    protected IASTboolean whatever =
            new ASTboolean("false");

    public Void visit(IASTtry iast, Context context)
            throws CompilationException {
        emit("{ struct ILP_catcher* current_catcher = ILP_current_catcher; \n");
        emit("  struct ILP_catcher new_catcher;  \n");
        emit("  if ( 0 == setjmp(new_catcher._jmp_buf) ) { \n");
        emit("      ILP_establish_catcher(&new_catcher); \n");
        Context cb = context.redirect(VoidDestination.VOID_DESTINATION);
        iast.getBody().accept(this, cb);
        emit("      ILP_current_exception = NULL; \n");
        emit("  }; \n");

        if ( iast.getCatcher() != null ) {
            IASTlambda catcher = iast.getCatcher();
            IASTvariable caughtVariable = catcher.getVariables()[0];
            emit("  ILP_reset_catcher(current_catcher); \n");
            emit("  if ( NULL != ILP_current_exception ) { \n");
            emit("      if ( 0 == setjmp(new_catcher._jmp_buf) ) { \n");
            emit("          ILP_establish_catcher(&new_catcher); \n");
            emit("          { ILP_Object ");
            emit(caughtVariable.getMangledName());
            emit(" = ILP_current_exception; \n");
            emit("            ILP_current_exception = NULL; \n");
            Context cc = context.redirect(VoidDestination.VOID_DESTINATION);
            catcher.getBody().accept(this, cc);
            emit("          } \n");
            emit("      }; \n");
            emit("  }; \n");
        }

        emit("  ILP_reset_catcher(current_catcher); \n");
        Context cc = context.redirect(VoidDestination.VOID_DESTINATION);
        if ( iast.getFinallyer() != null ) {
            iast.getFinallyer().accept(this, cc);
        }
        emit("  if ( NULL != ILP_current_exception ) { \n");
        emit("      ILP_throw(ILP_current_exception); \n");
        emit("  }; \n");
        whatever.accept(this, context);
        emit("}\n");
        return null;
    }
    
    // Class related
     
    public Void visit(IASTmethodDefinition iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTinstantiation iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public Void visit(IASTreadField iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTself iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public Void visit(IASTsend iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public Void visit(IASTclassDefinition iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
        
    public Void visit(IASTsuper iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTwriteField iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
}
