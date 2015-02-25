package com.paracamplus.ilp9.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Collection;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.paracamplus.ilp9.compiler.Compiler;
import com.paracamplus.ilp9.compiler.GlobalVariableEnvironment;
import com.paracamplus.ilp9.compiler.GlobalVariableStuff;
import com.paracamplus.ilp9.compiler.OperatorEnvironment;
import com.paracamplus.ilp9.compiler.OperatorStuff;
import com.paracamplus.ilp9.compiler.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp9.compiler.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp9.compiler.optimizer.IdentityOptimizer;
import com.paracamplus.ilp9.interfaces.IASTprogram;
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
        String compiled = compiler.compile(program);
        File cFile = changeSuffix(file, "c");
        FileTool.stuffFile(cFile, compiled);
        
        String indentProgram = "indent " + cFile.getAbsolutePath();
        ProgramCaller pcindent = new ProgramCaller(indentProgram);
        pcindent.run();
        System.out.println(FileTool.slurpFile(cFile));

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
        //Path currentRelativePath = Paths.get("");
        //String s = currentRelativePath.toAbsolutePath().toString();
        //System.err.println("Current relative path is: " + s);
        
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
