package com.paracamplus.ilp9.parser;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTsequence;

public interface IParser {
	IParserFactory getFactory();
	IASTprogram parse(Document dom) throws ParseException;
	
	// Utilities
	IAST findThenParseChild(NodeList nl, String childName)
	        throws ParseException;
	IAST[] findThenParseChildAsArray(Node n, String childName)
	        throws ParseException;
	IAST[] findThenParseChildAsArray(NodeList nl, String childName)
	        throws ParseException;
	IAST findThenParseChildAsUnique(Node n, String childName)
            throws ParseException;
	IAST findThenParseChildAsUnique(NodeList nl, String childName)
            throws ParseException;
	IASTsequence findThenParseChildAsSequence(NodeList nl, String childName)
	        throws ParseException;
	IASTsequence findThenParseChildAsSequence(Node n, String childName)
	        throws ParseException;
}
