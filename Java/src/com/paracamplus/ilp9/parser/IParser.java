package com.paracamplus.ilp9.parser;

import org.w3c.dom.Document;

import com.paracamplus.ilp9.interfaces.IASTprogram;

public interface IParser {
	IParserFactory getFactory();
	IASTprogram parse(Document dom) throws ParseException;
}
