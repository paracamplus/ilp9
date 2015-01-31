package com.paracamplus.ilp9.interpreter;

import java.util.Vector;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.parser.AbstractExtensibleParser;
import com.paracamplus.ilp9.parser.IParserFactory;

public class Parser extends AbstractExtensibleParser {

	public Parser(IParserFactory factory) {
		super(factory);
        addParser("alternative",         ASTalternative.class);
        addParser("sequence",            ASTsequence.class);
        addParser("loop",                ASTwhile.class);
        addParser("assignment",          ASTassignment.class);
        addParser("functionDefinition",  ASTfunctionDefinition.class);
        addParser("block",               ASTblock.class);
        addParser("variable",            ASTreference.class);
        addParser("invocation",          ASTinvocation.class);
        addParser("unaryOperation",      ASTunaryOperation.class);
        addParser("binaryOperation",     ASTbinaryOperation.class);
        addParser("integer",             ASTinteger.class);
        addParser("float",               ASTfloat.class);
        addParser("boolean",             ASTboolean.class);
        addParser("string",              ASTstring.class);
        addParser("try",                 ASTtry.class);
	}
}
