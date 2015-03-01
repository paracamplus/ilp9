/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.tools.test;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import com.paracamplus.ilp9.tools.FileTool;

public class FileToolTest extends TestCase {
    
    public void testInstantiation () {
        FileTool ft = new FileTool();
        assertNotNull(ft);
    }
    
    public void testSlurpFileOnEmptyFile ()
    throws IOException {
        File tmpFile = File.createTempFile("tsf0", "txt");
        String fileName = tmpFile.getAbsolutePath();
        final String s = FileTool.slurpFile(fileName);
        assertTrue(s.length() == 0 );
    }
   
    public void testStuffFile ()
    throws IOException {
        File tmpFile = File.createTempFile("tsf1", "txt");
        String fileName = tmpFile.getAbsolutePath();
        String content = "coucou";
        FileTool.stuffFile(tmpFile, content);
        final String s = FileTool.slurpFile(fileName);
        assertEquals(content, s);
    }
    
    public void testStuffFileChaineVide ()
    throws IOException {
        File tmpFile = File.createTempFile("tsf2", "txt");
        String fileName = tmpFile.getAbsolutePath();
        String content = "";
        FileTool.stuffFile(tmpFile, content);
        final String s = FileTool.slurpFile(fileName);
        assertEquals(content, s);
    }
    
}

//end of FileToolTest.java
