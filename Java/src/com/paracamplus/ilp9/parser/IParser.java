package com.paracamplus.ilp9.parser;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTprogram;

public interface IParser {
	IParserFactory getFactory();
	IASTprogram parse(Document dom) throws ParseException;
	
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
