package com.paracamplus.ilp9.parser;

import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTsequence;

public abstract class AbstractParser implements IParser {

	public AbstractParser(IParserFactory factory) {
		this.factory = factory;
	}
	public IParserFactory getFactory() {
		return factory;
	}
	private final IParserFactory factory;
	
    // Parsing entry points:
    
	public abstract IASTprogram parse(Document dom) throws ParseException;	
	public abstract IAST parse(Node e) throws ParseException;

	// Common utilities
	
    public IAST
    	findThenParseChild (final Node n, final String childName)
    throws ParseException {
        return findThenParseChild(n.getChildNodes(), childName);
    }

    public IAST
    	findThenParseChild (final NodeList nl, final String childName)
    throws ParseException {
        final int n = nl.getLength();
        for ( int i = 0 ; i<n ; i++ ) {
            final Node nd = nl.item(i);
            switch ( nd.getNodeType() ) {

            case Node.ELEMENT_NODE: {
                final Element e = (Element) nd;
                if ( childName.equals(e.getTagName()) ) {
                    return this.parse(e);
                }
                break;
            }

            default: {
            	// Just ignore everything but XML element.
            }
            }
        }
        final String msg = "No such child element " + childName;
        throw new ParseException(msg);
    }

    public IAST[] findThenParseChildAsArray (
            final Node n, final String childName)
    throws ParseException {
        return findThenParseChildAsArray(n.getChildNodes(), childName);
    }

    public IAST[] findThenParseChildAsArray (
            final NodeList nl, final String childName)
    throws ParseException {
        final int n = nl.getLength();
        for ( int i = 0 ; i<n ; i++ ) {
            final Node nd = nl.item(i);
            switch ( nd.getNodeType() ) {

            case Node.ELEMENT_NODE: {
                final Element e = (Element) nd;
                if ( childName.equals(e.getTagName()) ) {
                    return this.parseAll(e.getChildNodes());
                }
                break;
            }

            default: {
            	// Just ignore everything but XML element.
            }
            }
        }
        final String msg = "No such node " + childName;
        throw new ParseException(msg);
    }
    
    public IAST findThenParseChildAsUnique (
            final Node n,
            final String childName)
            throws ParseException {
        return findThenParseChildAsUnique(n.getChildNodes(), childName);
    }

    public IAST findThenParseChildAsUnique (
            final NodeList nl,
            final String childName)
            throws ParseException {
        final IAST[] results =
            findThenParseChildAsArray(nl, childName);
        if ( 1 == results.length ) {
            return results[0];
        } else {
            final String msg = "Should be an unique DOM node!";
            throw new ParseException(msg);
        }
    }
    
    public IAST[] parseAll (NodeList nl)
    throws ParseException {
        final Vector<IAST> result = new Vector<>();
        final int n = nl.getLength();
        LOOP:
            for ( int i = 0 ; i<n ; i++ ) {
                final Node nd = nl.item(i);
                switch ( nd.getNodeType() ) {

                case Node.ELEMENT_NODE: {
                	final Element e = (Element) nd;
                    final IAST p = this.parse(e);
                    result.add(p);
                    continue LOOP;
                }

                default: {
                	// Just ignore everything but XML element.
                }
                }
            }
        return result.toArray(new IAST[0]);
    }
    
    public IASTsequence findThenParseChildAsSequence (
            NodeList nl, String childName)
    throws ParseException {
        IAST[] iasts = findThenParseChildAsArray(nl, childName);
        Vector<IASTexpression> viasts = new Vector<>();
        for (IAST iast : iasts) {
        	if (iast instanceof IASTexpression) {
        		viasts.add((IASTexpression) iast);
        	} else {
        		final String msg = "Not an IASTexpression " + iast;
        		throw new ParseException(msg);
        	}
        }
        IASTexpression[] v = viasts.toArray(new IASTexpression[0]);
        return getFactory().newSequence(v);
    }

    public IASTsequence findThenParseChildAsSequence (
            Node n, String childName)
    throws ParseException {
        return findThenParseChildAsSequence(n.getChildNodes(), childName);
    }
    
    public IASTsequence parseChildrenAsSequence (NodeList nl)
    throws ParseException {
        IAST[] iasts = parseAll(nl);
        Vector<IASTexpression> viasts = new Vector<>();
        for (IAST iast : iasts) {
        	if ( iast instanceof IASTexpression) {
        		viasts.add((IASTexpression) iast);
        	} else {
        		final String msg = "Not an IASTexpression " + iast;
        		throw new ParseException(msg);
        	}
        }
        IASTexpression[] v = viasts.toArray(new IASTexpression[0]);
        return getFactory().newSequence(v);
    }
}
