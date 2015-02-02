package com.paracamplus.ilp9.interpreter;

import java.util.List;
import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.parser.AbstractExtensibleParser;
import com.paracamplus.ilp9.parser.IParserFactory;
import com.paracamplus.ilp9.parser.ParseException;

public class Parser extends AbstractExtensibleParser {

	public Parser(IParserFactory factory) {
		super(factory);
        addMethod("alternative", Parser.class);
        addMethod("sequence", Parser.class);
        addMethod("integer", Parser.class);
	}

    public static IASTexpression narrowToIASTexpression (IAST iast) 
            throws ParseException {
        if ( iast != null && iast instanceof IASTexpression ) {
            return (IASTexpression) iast;
        } else {
            final String msg = "Not an ASTexpression " + iast;
            throw new ParseException(msg);
        }
    }
    
    public IASTprogram parse (Document d) throws ParseException {
        final Element e = d.getDocumentElement();
        final IAST[] iasts = parseAll(e.getChildNodes());
        final List<IASTfunctionDefinition> functionDefinitions = new Vector<>();
        final List<IASTclassDefinition> classDefinitions = new Vector<>();
        final List<IASTexpression> expressions = new Vector<>();
        for ( IAST iast : iasts ) {
            if ( iast instanceof IASTfunctionDefinition ) {
                functionDefinitions.add((IASTfunctionDefinition) iast);
            } else if ( iast instanceof IASTclassDefinition ) {
                classDefinitions.add((IASTclassDefinition) iast);
            } else if ( iast instanceof IASTexpression ) {
                expressions.add((IASTexpression) iast);
            } else {
                final String msg = "Should never occur!";
                assert false : msg;
                throw new ParseException(msg);
            }
        }
        IASTfunctionDefinition[] defs =
            functionDefinitions.toArray(new IASTfunctionDefinition[0]);
        IASTclassDefinition[] clazzes =
            classDefinitions.toArray(new IASTclassDefinition[0]);
        IASTexpression[] exprs = 
            expressions.toArray(new IASTexpression[0]);
        IASTsequence body = getFactory().newSequence(exprs);
        return getFactory().newProgram(defs, clazzes, body);
    }

	public IASTalternative alternative (Element e) throws ParseException {
        final NodeList nl = e.getChildNodes();
        IAST iastc = findThenParseChildAsUnique(nl, "condition");
        IASTexpression condition = narrowToIASTexpression(iastc);
        IAST iaste = findThenParseChildAsSequence(nl, "consequence");
        IASTexpression consequence = narrowToIASTexpression(iaste);
        IAST iasta = findThenParseChildAsSequence(nl, "alternant");
        try {
            IASTexpression alternant = narrowToIASTexpression(iasta);
            return getFactory().newAlternative(
                    condition, consequence, alternant);
        } catch (ParseException exc) {
            return getFactory().newAlternative(
                    condition, consequence, null);
        }
    }
	
	public IASTsequence sequence (Element e) throws ParseException {
	    final IAST[] iasts = parseAll(e.getChildNodes());
	    List<IASTexpression> exprs = new Vector<>();
	    for ( IAST iast : iasts ) {
	        if ( iast instanceof IASTexpression ) {
	            exprs.add((IASTexpression) iast);
	        }
	    }
	    IASTexpression[] expressions = exprs.toArray(new IASTexpression[0]);
	    return getFactory().newSequence(expressions);
	}
	
	public IASTinteger integer (Element e) throws ParseException {
	    final String description = e.getAttribute("value");
	    return getFactory().newIntegerConstant(description);
	}


}
