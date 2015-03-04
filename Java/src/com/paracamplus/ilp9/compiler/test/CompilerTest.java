/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.paracamplus.ilp9.compiler.CompilationException;
import com.paracamplus.ilp9.compiler.Compiler;
import com.paracamplus.ilp9.compiler.GlobalVariableEnvironment;
import com.paracamplus.ilp9.compiler.GlobalVariableStuff;
import com.paracamplus.ilp9.compiler.OperatorEnvironment;
import com.paracamplus.ilp9.compiler.OperatorStuff;
import com.paracamplus.ilp9.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp9.compiler.interfaces.IASTCmethodDefinition;
import com.paracamplus.ilp9.compiler.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp9.compiler.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp9.compiler.optimizer.IdentityOptimizer;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.interfaces.IASTvisitor;
import com.paracamplus.ilp9.parser.IParser;
import com.paracamplus.ilp9.parser.IParserFactory;
import com.paracamplus.ilp9.tools.FileTool;
import com.paracamplus.ilp9.tools.Input;
import com.paracamplus.ilp9.tools.InputFromFile;
import com.paracamplus.ilp9.tools.ProgramCaller;

@RunWith(Parameterized.class)
public class CompilerTest {
    
    protected static String rngFileName = "grammar9.rng";
    protected static String samplesDirName = "Samples";
    protected static String pattern = "ur?[0-78]\\d*-[123456](gfv)?";
    
    public CompilerTest(final File file) {
        this.file = file;
        IParserFactory factory = new com.paracamplus.ilp9.ast.ASTfactory();
        this.parser = new com.paracamplus.ilp9.ast.Parser(factory);
    }
    protected File file;
    
    public void setParser (IParser parser) {
        this.parser = parser;
    }
    protected IParser parser;
    
    protected IASTCclassDefinition createObjectClass() {
        IASTCclassDefinition oc = new IASTCclassDefinition() {
            public String getName() {
                return "Object";
            }
            public IASTCclassDefinition getSuperClass() {
                throw new RuntimeException("No super class for Object");
            }
            public String[] getProperFieldNames() {
                return new String[0];
            }
            public String[] getTotalFieldNames() {
                return getProperFieldNames();
            }
            public int getFieldOffset(String fieldName)
                    throws CompilationException {
                String msg = "No such field " + fieldName;
                throw new CompilationException(msg);
            }
            public IASTCmethodDefinition[] getProperMethodDefinitions() {
                return predefinedMethods;
            }
            public IASTCmethodDefinition[] getNewProperMethodDefinitions() {
                return predefinedMethods;
            }
            protected IASTCmethodDefinition[] predefinedMethods =
                    new IASTCmethodDefinition[2];
            public IASTCmethodDefinition[] getTotalMethodDefinitions() {
                return getProperMethodDefinitions();
            }
        };
        IASTCmethodDefinition mdprint = new ASTCprimitiveMethodDefinition(
                "print", "ILPm_print", oc);
        oc.getProperMethodDefinitions()[0] = mdprint;
        IASTCmethodDefinition mdclassOf = new ASTCprimitiveMethodDefinition(
                "classOf", "ILPm_classOf", oc);
        oc.getProperMethodDefinitions()[1] = mdclassOf;
        return oc;
    }
    
    public static class ASTCprimitiveMethodDefinition 
    implements IASTCmethodDefinition {
        public ASTCprimitiveMethodDefinition(
                String methodName,
                String cName,
                IASTCclassDefinition clazz ) {
            this.methodName = methodName;
            this.cName = cName;
            this.clazz = clazz;
        }
        private final String methodName;
        private final String cName;
        private final IASTCclassDefinition clazz;

        public String getMethodName() {
            return methodName;
        }
        public String getName() {
            return cName;
        }
        public String getCName() {
            return cName;
        }
        // FIXME Missing getMangledName() ???
        public IASTCclassDefinition getDefiningClass() {
            return clazz;
        }
        public String getDefiningClassName() {
            return clazz.getName();
        }
        public IASTvariable getFunctionVariable() {
            return methodVariable;
        }
        IASTvariable methodVariable = new IASTvariable() {
            public String getName() {
                return cName;
            }
            public <Result, Data, Anomaly extends Throwable> Result 
            accept(IASTvisitor<Result, Data, Anomaly> visitor, Data data)
                    throws Anomaly {
                return visitor.visit(this, data);
            }
        };
        public IASTvariable[] getVariables() {
            throw new RuntimeException("NYI"); // FIXME
        }
        public IASTexpression getBody() {
            throw new RuntimeException("NYI"); // FIXME
        }
        public Set<IASTvariable> getClosedVariables() {
            return closedVariables;
        }
        public void setClosedVariables(Set<IASTClocalVariable> newvars) {
            this.closedVariables.addAll(closedVariables);
        }
        private final Set<IASTvariable> closedVariables =
                new HashSet<>();
        public IASTCmethodDefinition findSuperMethod() 
                throws CompilationException {
            String msg = "No super method";
            throw new CompilationException(msg);
        }
    }
    
    @Test
    public void processFile () throws Throwable {
        System.err.println("Testing " + file.getAbsolutePath() + " ...");
        assertTrue(file.exists());
        Input input = new InputFromFile(file);
        parser.setInput(input);
        File rngFile = new File(rngFileName);
        parser.setGrammar(rngFile);
        IASTprogram program = parser.getProgram();
        
        IOperatorEnvironment ioe = new OperatorEnvironment();
        OperatorStuff.fillUnaryOperators(ioe);
        OperatorStuff.fillBinaryOperators(ioe);
        IGlobalVariableEnvironment gve = new GlobalVariableEnvironment();
        GlobalVariableStuff.fillGlobalVariables(gve);
        Compiler compiler = new Compiler(ioe, gve);
        compiler.setOptimizer(new IdentityOptimizer());
        IASTCclassDefinition objectClass = createObjectClass();
        String compiled = compiler.compile(program, objectClass);
        File cFile = changeSuffix(file, "c");
        FileTool.stuffFile(cFile, compiled);

        try {
          String indentProgram = "indent " + cFile.getAbsolutePath();
          ProgramCaller pcindent = new ProgramCaller(indentProgram);
          pcindent.run();
          System.out.println(FileTool.slurpFile(cFile));
        } catch (Exception exc) {
          // program 'indent' is probably absent, ignore!
          System.out.println(compiled);
        }

        String compileProgram = "bash C/compileThenRun.sh +gc "
            + cFile.getAbsolutePath();
        ProgramCaller pc = new ProgramCaller(compileProgram);
        pc.setVerbose();
        pc.run();
        assertEquals("Comparing return code", 0, pc.getExitValue());
        String executionPrinting = pc.getStdout().trim();
        checkPrintingAndResult(executionPrinting);
    }
    
    public void checkPrintingAndResult(String actual) throws IOException {
        String expectedPrinting = readExpectedPrinting(file);
        String expectedResult = readExpectedResult(file);
        String expected = expectedPrinting + expectedResult;
        assertEquals("Compare", expected, actual);
    }
    
    @Parameters(name = "{0}")
    public static Collection<File[]> data() throws Exception {
        final Pattern p = Pattern.compile("^" + pattern + ".xml$");
        final FilenameFilter ff = new FilenameFilter() {
            public boolean accept (File dir, String name) {
                final Matcher m = p.matcher(name);
                return m.matches();
            }
        };
        File samplesDir = new File(samplesDirName);
        final File[] testFiles = samplesDir.listFiles(ff);
        assertNotNull(testFiles);
        
        if ( testFiles.length == 0 ) {
            final String msg = "Cannot find a single test like " + pattern;
            throw new RuntimeException(msg);
        }

        // Old way before Java8:
//        java.util.Arrays.sort(testFiles,
//                new java.util.Comparator<java.io.File>() {
//            public int compare (java.io.File f1, java.io.File f2) {
//                return f1.getName().compareTo(f2.getName());
//            }
//        });
        java.util.Arrays.sort(testFiles,
                (f1, f2) -> f1.getName().compareTo(f2.getName()));

       Collection<File[]> result = new Vector<>();
        for ( final File f : testFiles ) {
            result.add(new File[]{ f });
        }
        return result;
    }
    
    public static File changeSuffix(File file, String suffix) {
        String parent = file.getParent();
        String name = file.getName();
        String basename;
        int dotIndex = name.lastIndexOf('.');
        if (dotIndex >= 0) {
            basename = name.substring(0, dotIndex);
        } else {
            basename = name;
        }
        String newName = parent + File.separator + basename + '.' + suffix;
        return new File(newName);
    }

    public static String readExpectedPrinting (File file)
      throws IOException {
      File resultFile = changeSuffix(file, "print");
      assertTrue(file.exists());
      return FileTool.slurpFile(resultFile).trim();
    }

    public static String readExpectedResult (File file)
      throws IOException {
      File resultFile = changeSuffix(file, "result");
      assertTrue(file.exists());
      return FileTool.slurpFile(resultFile).trim();
    }

}
