package com.paracamplus.ilp9.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;

public class FileTool {
    
    public static String slurpFile (final File file)
            throws IOException {
        final int length = (int) file.length();
        final char[] buffer = new char[length];
        try (final FileReader fr = new FileReader(file)) {
            int offset = 0;
            while ( offset < length ) {
                int read = fr.read(buffer, offset, length-offset);
                offset += read;
            }
        }
        return new String(buffer);
    }
    
    public static String slurpFile (final Reader fr)
    throws IOException {
        final StringBuffer sb = new StringBuffer();
        final BufferedReader br = new BufferedReader(fr);
        final char[] buffer = new char[4096];
        while ( true ) {
            int count = br.read(buffer);
            if (count < 0) {
                return sb.toString();
            }
            sb.append(buffer, 0, count);
        }
    }

    public static String slurpFile (final String filename)
    throws IOException {
        return FileTool.slurpFile(new File(filename));
    }
    
    public static void stuffFile (final File file, final String s)
    throws IOException {
        //final FileWriter fw = new FileWriter(file);
        //try {
        //    fw.write(s, 0, s.length());
        //} finally {
        //    fw.close();
        //}
        // New way since Java7:
        try (final FileWriter fw = new FileWriter(file)) {
            fw.write(s, 0, s.length());
        }
    }

}
