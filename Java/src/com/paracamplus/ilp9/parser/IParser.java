package com.paracamplus.ilp9.parser;

import java.io.File;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.tools.Input;

public interface IParser {
	IParserFactory getFactory();
    IASTprogram getProgram() throws ParseException;
    
    void setInput(Input input);
    void setGrammar(File rngFile);
	
	// Utilities
	Element findChild (final Node n, final String childName)
	        throws ParseException;
    Element findChild (final NodeList nl, final String childName)
            throws ParseException;
    
	IAST findThenParseChild(NodeList nl, String childName)
	        throws ParseException;
	IAST[] findThenParseChildAsArray(Node n, String childName)
	        throws ParseException;
	IAST[] findThenParseChildAsArray(NodeList nl, String childName)
	        throws ParseException;
	IASTexpression[] findThenParseChildAsExpressions(Node n, String childName)
            throws ParseException;
	IASTexpression[] findThenParseChildAsExpressions(NodeList nl, String childName)
            throws ParseException;
}
