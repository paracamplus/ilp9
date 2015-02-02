package com.paracamplus.ilp9.tools;

import java.io.File;

import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.parser.IParser;
import com.paracamplus.ilp9.parser.ParseException;

public interface IProcess {
    IASTprogram getProgram() throws ParseException;
    void setInput(Input input);
    void setGrammar(File rngFile);
    void setParser(IParser parser);
}
