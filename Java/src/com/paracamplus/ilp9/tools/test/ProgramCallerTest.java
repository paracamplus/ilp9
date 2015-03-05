/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.tools.test;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.TestCase;

import com.paracamplus.ilp9.tools.ProgramCaller;

public class ProgramCallerTest extends TestCase {

    public void testProgramCallerInexistentVerbose () {
        final String programName = "lasdljsdfousadfl lsjd";
        ProgramCaller pc = new ProgramCaller(programName);
        assertNotNull(pc);
        pc.setVerbose();
        pc.run();
        assertTrue(pc.getExitValue() != 0);
    }

    public void testProgramCallerInexistent () {
        final String programName = "lasdljsdfousadfl lsjd";
        ProgramCaller pc = new ProgramCaller(programName);
        pc.run();
        final String result = pc.getStderr();
        System.err.println("Result: " + result); // DEBUG
        Pattern p = Pattern.compile(".*(not found|Cannot run).*");
        Matcher m = p.matcher(result);
        assertTrue(m.matches());
        assertTrue(pc.getExitValue() != 0);
    }

    public void testProgramCallerEchoVerbose () {
        final ProgramCaller pc = new ProgramCaller("echo cou cou  cou");
        assertNotNull(pc);
        pc.setVerbose();
        pc.run();
        assertTrue(pc.getExitValue() == 0);
        final String result = pc.getStdout();
        System.err.println("Result: " + result); // DEBUG
        assertNotNull(result);
        assertTrue(result.length() > 0);
        assertEquals(result.trim(), "cou cou cou");
    }

    public void testProgramCallerEcho () {
        final ProgramCaller pc = new ProgramCaller("echo cou cou  cou");
        assertNotNull(pc);
        pc.run();
        assertTrue(pc.getExitValue() == 0);
        final String result = pc.getStdout();
        System.err.println("Result: " + result); // DEBUG
        assertNotNull(result);
        assertTrue(result.length() > 0);
        assertEquals(result.trim(), "cou cou cou");
    }

    public void testProgramCallerGccOnStdout () {
        final ProgramCaller pc = new ProgramCaller("gcc -v");
        assertNotNull(pc);
        pc.run();
        assertTrue(pc.getExitValue() == 0);
        final String result = pc.getStdout();
        System.err.println("Result: " + result); // DEBUG
        assertNotNull(result);
        assertTrue(result.length() == 0);
        assertEquals(result.trim(), "");
    }

    // MacOSX says clang instead of gcc:
    private String gccPattern = ".*(clang|gcc).*";
    
    public void testProgramCallerGccOnStderr () {
        final ProgramCaller pc = new ProgramCaller("gcc -c unexistent.c");
        assertNotNull(pc);
        pc.run();
        assertTrue(pc.getExitValue() != 0);
        final String errors = pc.getStderr();
        System.err.println("Errors: " + errors); // DEBUG
        assertNotNull(errors);
        assertTrue(errors.length() > 0);
        Pattern p = Pattern.compile(gccPattern, Pattern.DOTALL);
        Matcher m = p.matcher(errors);
        assertTrue(m.matches());
    }

    public void testProgramCallerGccOnStderrVerbose () {
        final ProgramCaller pc = new ProgramCaller("gcc -c unexistent.c");
        assertNotNull(pc);
        pc.setVerbose();
        pc.run();
        assertTrue(pc.getExitValue() != 0);
        final String errors = pc.getStderr();
        System.err.println("Errors: " + errors); // DEBUG
        assertNotNull(errors);
        assertTrue(errors.length() > 0);
        Pattern p = Pattern.compile(gccPattern, Pattern.DOTALL);
        Matcher m = p.matcher(errors);
        assertTrue(m.matches());
    }

    public void testProgramCallerVerbose () {
        final ProgramCaller pc = new ProgramCaller("echo See what happens");
        assertNotNull(pc);
        pc.setVerbose();
        pc.run();
        assertTrue(pc.getExitValue() == 0);
        final String result = pc.getStdout();
        System.err.println("Result: " + result); // DEBUG
        assertNotNull(result);
        assertTrue(result.length() > 0);
        Pattern p1 = Pattern.compile(".*See what happens.*");
        Matcher m1 = p1.matcher(result.trim());
        assertTrue(m1.matches());
        final String errors = pc.getStderr();
        System.err.println("Errors: " + errors); // DEBUG
        assertNotNull(errors);
        assertTrue(errors.length() == 0);
    }

    // testing multiple concurrent tasks

    public void testMultipleProgramCallers () {
        System.err.println("\nATTENTION: this test lasts for 10 seconds...");
        final int max = 2;
        final ProgramCaller[] pcs = new ProgramCaller[max];
        for ( int i = max ; i>0 ; i-- ) {
            pcs[i-1] = new ProgramCaller("sleep " + i);
            pcs[i-1].run();
        }
        for ( int i = 0 ; i<max ; i++ ) {
            assertTrue(pcs[i].getExitValue() == 0);
        }
    }

}

// end of ProgramCallerTest.java
