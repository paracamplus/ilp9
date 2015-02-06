package com.paracamplus.ilp9.compiler;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.atomic.AtomicInteger;

import com.paracamplus.ilp9.ast.ASTvariable;
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
import com.paracamplus.ilp9.interfaces.IASTreference;
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
        public Context (IDestination destination,
                        ILexicalEnvironment lexenv ) {
            this.destination = destination;
            this.lexenv = lexenv;
        }
        public IDestination destination;
        public ILexicalEnvironment lexenv;
        public static AtomicInteger counter = new AtomicInteger(0);
        
        public IASTvariable newTemporaryVariable () {
            int i = counter.incrementAndGet();
            return new ASTvariable("ILPtmp" + i);
        }
        
        public Context extend (IASTvariable v) {
            return new Context(destination, lexenv.extend(v, null));
        }
        
        public Context redirect (IDestination d) {
            if ( d == destination ) {
                return this;
            } else {
                return new Context(d, lexenv);
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
   
    public String compile(IASTprogram program) throws CompilationException {
        
        // analysis on program FIXME        
      
        Context context = new Context(NoDestination.NO_DESTINATION,
                                      LexicalEnvironment.EMPTY);
        StringWriter sw = new StringWriter();
        try {
            out = new BufferedWriter(sw);
            visit(program, context);
            out.flush();
        } catch (IOException exc) {
            throw new CompilationException(exc);
        }
        return sw.toString();
    }
    private Writer out;

    //
    
    public Void visit(IASTprogram iast, Context context)
            throws CompilationException {
        emit(cProgramPrefix);
        
        // TODO emit global variables
        
        emit(cFunctionsPrefix);
        Context c = context.redirect(NoDestination.NO_DESTINATION);
        for ( IASTfunctionDefinition ifd : iast.getFunctionDefinitions() ) {
            ifd.accept(this, c);
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
            + "#include \"ilp.h\" \n";
    protected String cFunctionsPrefix = "\n"
            + "/* Global functions */ \n";
    protected String cFunctionsSuffix = "\n";
    protected String cBodyPrefix = "\n"
            + "ILP_Object ilp_program () \n"
            + "{ \n";
    protected String cBodySuffix = "\n"
            + "} \n";
    protected String cProgramSuffix = "\n"
            + "int main (int argc, char *argv[]) \n"
            + "{ \n"
            + "  ILP_print(ilp_program()); \n"
            + "  ILP_newline(); \n"
            + "  return EXIT_SUCCESS; \n"
            + "} \n";

    public Void visit(IASTsequence iast, Context context)
            throws CompilationException {
        IASTvariable tmp = context.newTemporaryVariable();
        context = context.extend(tmp);
        IASTexpression[] expressions = iast.getExpressions();
        Context c = context.redirect(new AssignDestination(tmp));
        emit("{ \n");
        emit("  ILP_Object " + tmp.getMangledName() + "; \n");
        for ( IASTexpression expr : expressions ) {
            expr.accept(this, c);
            //emit("; \n");
        }
        emit(context.destination.compile());
        tmp.accept(this, context);
        emit("; \n} \n");
        return null;
    }

    public Void visit(IASTvariable iast, Context context)
            throws CompilationException {
        emit(iast.getMangledName());
        return null;
    }

    public Void visit(IASTreference iast, Context context)
            throws CompilationException {
        emit(context.destination.compile());
        iast.getVariable().accept(this, context);
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
        context = context.extend(tmp1);
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(tmp1));
        iast.getOperand().accept(this, c1);
        String cName = operatorEnvironment.getUnaryOperator(iast.getOperator());
        emit(context.destination.compile());
        emit(cName);
        emit("(");
        tmp1.accept(this, context);
        emit(");\n");
        emit("} \n");
        return null;
    }

    public Void visit(IASTbinaryOperation iast, Context context)
            throws CompilationException {
        IASTvariable tmp1 = context.newTemporaryVariable();
        context = context.extend(tmp1);
        IASTvariable tmp2 = context.newTemporaryVariable();
        context = context.extend(tmp2);
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
        tmp1.accept(this, context);
        emit(", ");
        tmp2.accept(this, context);
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
        context = context.extend(tmp1);
        emit("{ \n");
        emit("  ILP_Object " + tmp1.getMangledName() + "; \n");
        Context c = context.redirect(new AssignDestination(tmp1));
        iast.getCondition().accept(this, c);
        emit("  if ( ILP_isEquivalentToTrue(");
        emit(tmp1.getMangledName());
        emit(" ) ) {\n");
        iast.getConsequence().accept(this, context);
        emit("\n  } else {\n");
        iast.getAlternant().accept(this, context);
        emit("\n  }\n}\n");
        return null;
    }
    
    public Void visit(IASTassignment iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTblock iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTbinding iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTcodefinitions iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
     
    public Void visit(IASTfunctionDefinition iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
      
    public Void visit(IASTinvocation iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTlambda iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }
    
    public Void visit(IASTloop iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
    }

    public Void visit(IASTtry iast, Context context)
            throws CompilationException {
        // TODO Auto-generated method stub
        throw new RuntimeException("NYI");
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
