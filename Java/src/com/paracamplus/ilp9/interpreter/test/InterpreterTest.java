/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interpreter.ClassEnvironment;
import com.paracamplus.ilp9.interpreter.EmptyLexicalEnvironment;
import com.paracamplus.ilp9.interpreter.GlobalVariableEnvironment;
import com.paracamplus.ilp9.interpreter.GlobalVariableStuff;
import com.paracamplus.ilp9.interpreter.Interpreter;
import com.paracamplus.ilp9.interpreter.OperatorEnvironment;
import com.paracamplus.ilp9.interpreter.OperatorStuff;
import com.paracamplus.ilp9.interpreter.interfaces.IClassEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp9.interpreter.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp9.parser.IParser;
import com.paracamplus.ilp9.parser.IParserFactory;
import com.paracamplus.ilp9.tools.FileTool;
import com.paracamplus.ilp9.tools.Input;
import com.paracamplus.ilp9.tools.InputFromFile;

@RunWith(Parameterized.class)
public class InterpreterTest {
    
    protected static String rngFileName = "grammar9.rng";
    protected static String samplesDirName = "Samples";
    protected static String pattern = "ur?[0-8]\\d*-[123456]";
    
    public InterpreterTest(final File file) {
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
        
        IGlobalVariableEnvironment gve = new GlobalVariableEnvironment();
        stdout = new StringWriter();
        GlobalVariableStuff.fillGlobalVariables(gve, stdout);
        IOperatorEnvironment oe = new OperatorEnvironment();
        OperatorStuff.fillUnaryOperators(oe);
        OperatorStuff.fillBinaryOperators(oe);
        IClassEnvironment ice = new ClassEnvironment(stdout);
        Interpreter interpreter = new Interpreter(gve, oe, ice);
        ILexicalEnvironment lexenv = new EmptyLexicalEnvironment();
        Object value = interpreter.visit(program, lexenv);
        String printing = stdout.toString();
        System.out.println("  Value: " + value);
        if ( ! "".equals(printing) ) {
            System.out.println("  Printing: " + printing);
        }
        checkResult(value);
        checkPrinting(printing);
    }
    private Writer stdout;
    
    protected static Object normalizeResult(Object value) {
        if (value instanceof BigInteger) {
            return ((BigInteger)value).intValue();
        } else if ( value instanceof BigDecimal ) {
            return ((BigDecimal)value).doubleValue();
        } else {
            return value;
        }
    }
    
    public void checkResult (Object value) throws IOException {
        String expectedResult = readExpectedResult(file);
        value = normalizeResult(value);
        if ( value instanceof Double ) {
            double expected = Double.parseDouble(expectedResult);
            assertEquals("Comparing double results", 
                    expected,
                    (double)value, 
                    0.01);
        } else if ( value instanceof Integer ) {
            assertEquals("Comparing integer results",
                    value.toString(),
                    expectedResult.toString());
        } else {
            assertEquals("Comparing results", 
                    value.toString(),
                    expectedResult);
        }
    }
    
    public void checkPrinting(String printing) throws IOException {
        String expectedPrinting = readExpectedPrinting(file);
        assertEquals("Comparing printings", printing, expectedPrinting);
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
