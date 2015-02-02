package com.paracamplus.ilp9.interpreter;

import java.io.File;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.parser.IParser;
import com.paracamplus.ilp9.parser.ParseException;
import com.paracamplus.ilp9.tools.IProcess;
import com.paracamplus.ilp9.tools.Input;
import com.thaiopensource.validate.ValidationDriver;

public class Process implements IProcess {
    
    public void setInput(Input input) {
        this.input = input;
    }
    private Input input;
    
    public void setGrammar (File rngFile) {
        this.rngFile = rngFile;
    }
    private File rngFile;
    
    public void setParser (IParser parser) {
        this.parser = parser;
    }
    private IParser parser;
    
    public IASTprogram getProgram() throws ParseException {
        try {
            final String programText = input.getText();
            final String rngFilePath = rngFile.getAbsolutePath();
            final InputSource isg = ValidationDriver.fileInputSource(rngFilePath);
            final ValidationDriver vd = new ValidationDriver();
            vd.loadSchema(isg);
            
            InputSource is = new InputSource(new StringReader(programText));
            if ( ! vd.validate(is) ) {
                throw new ParseException("Invalid XML program!");
            }

            final DocumentBuilderFactory dbf =
                DocumentBuilderFactory.newInstance();
            final DocumentBuilder db = dbf.newDocumentBuilder();
            // the previous value of is is totally drained!
            is = new InputSource(new StringReader(programText));
            final Document document = db.parse(is);
            program = parser.parse(document);
            return program;
        } catch (Exception e) {
            throw new ParseException(e);
        }
    }
    private IASTprogram program;
    
    public Object eval() {
        return null;
    }
}
