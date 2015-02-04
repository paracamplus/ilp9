package com.paracamplus.ilp9.ast;

import java.util.List;
import java.util.Vector;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTalternative;
import com.paracamplus.ilp9.interfaces.IASTassignment;
import com.paracamplus.ilp9.interfaces.IASTbinaryOperation;
import com.paracamplus.ilp9.interfaces.IASTblock;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTboolean;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfloat;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTinteger;
import com.paracamplus.ilp9.interfaces.IASTinvocation;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTloop;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTsequence;
import com.paracamplus.ilp9.interfaces.IASTstring;
import com.paracamplus.ilp9.interfaces.IASTtry;
import com.paracamplus.ilp9.interfaces.IASTunaryOperation;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.parser.AbstractExtensibleParser;
import com.paracamplus.ilp9.parser.IParserFactory;
import com.paracamplus.ilp9.parser.ParseException;

public class Parser extends AbstractExtensibleParser {

	public Parser(IParserFactory factory) {
		super(factory);
        addMethod("alternative", Parser.class);
        addMethod("sequence", Parser.class);
        addMethod("integerConstant", Parser.class, "integer");
        addMethod("floatConstant", Parser.class, "float");
        addMethod("stringConstant", Parser.class, "string");
        addMethod("booleanConstant", Parser.class, "boolean");
        addMethod("unaryOperation", Parser.class);
        addMethod("binaryOperation", Parser.class);
        addMethod("block", Parser.class);
        addMethod("binding", Parser.class);
        addMethod("variable", Parser.class);
        addMethod("invocation", Parser.class);
        addMethod("assignment", Parser.class);
        addMethod("loop", Parser.class);
        addMethod("functionDefinition", Parser.class);
        addMethod("tryInstruction", Parser.class, "try");
        addMethod("lambda", Parser.class);
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
    
    public static IASTvariable narrowToIASTvariable (IAST iast) 
            throws ParseException {
        if ( iast != null && iast instanceof IASTvariable) {
            return (IASTvariable) iast;
        } else {
            final String msg = "Not an ASTvariable" + iast;
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
            if ( iast != null && iast instanceof IASTfunctionDefinition ) {
                functionDefinitions.add((IASTfunctionDefinition) iast);
            } else if ( iast != null && iast instanceof IASTclassDefinition ) {
                classDefinitions.add((IASTclassDefinition) iast);
            } else if ( iast != null && iast instanceof IASTexpression ) {
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
        IAST iastc = findThenParseChildContent(e, "condition");
        IASTexpression condition = narrowToIASTexpression(iastc);
        IASTexpression[] iaste = 
                findThenParseChildAsExpressions(e, "consequence");
        IASTexpression consequence = getFactory().newSequence(iaste);
        try {
            IASTexpression[] iasta = 
                    findThenParseChildAsExpressions(e, "alternant");
            IASTexpression alternant = getFactory().newSequence(iasta);
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
	        if ( iast != null && iast instanceof IASTexpression ) {
	            exprs.add((IASTexpression) iast);
	        }
	    }
	    IASTexpression[] expressions = exprs.toArray(new IASTexpression[0]);
	    return getFactory().newSequence(expressions);
	}
	
	public IASTinteger integerConstant (Element e) throws ParseException {
	    final String description = e.getAttribute("value");
	    return getFactory().newIntegerConstant(description);
	}

    public IASTfloat floatConstant (Element e) throws ParseException {
        final String description = e.getAttribute("value");
        return getFactory().newFloatConstant(description);
    }

    public IASTstring stringConstant (Element e) throws ParseException {
        final String description = e.getTextContent();
        return getFactory().newStringConstant(description);
    }

    public IASTboolean booleanConstant (Element e) throws ParseException {
        final String description = e.getAttribute("value");
        return getFactory().newBooleanConstant(description);
    }
    
    public IASTexpression variable (Element e) throws ParseException {
        final String name = e.getAttribute("name");
        IASTvariable variable = getFactory().newVariable(name);
        return getFactory().newReference(variable);
    }
    
    public IASTunaryOperation unaryOperation (Element e) 
            throws ParseException {
        IAST iast = findThenParseChildContent(e, "operand");
        IASTexpression operand = narrowToIASTexpression(iast);
        String operatorName = e.getAttribute("operator");
        IASToperator operator = getFactory().newOperator(operatorName);
        return getFactory().newUnaryOperation(operator, operand);
    }

    public IASTbinaryOperation binaryOperation (Element e) 
            throws ParseException {
        IAST iast1 = findThenParseChildContent(e, "leftOperand");
        IASTexpression operand1 = narrowToIASTexpression(iast1);
        IAST iast2 = findThenParseChildContent(e, "rightOperand");
        IASTexpression operand2 = narrowToIASTexpression(iast2);
        String operatorName = e.getAttribute("operator");
        IASToperator operator = getFactory().newOperator(operatorName);
        return getFactory().newBinaryOperation(operator, operand1, operand2);
    }
    
    public IASTblock block (Element e) throws ParseException {
        IAST[] iastbindings = findThenParseChildAsArray(e, "bindings");
        List<IASTbinding> b = new Vector<>();
        for ( IAST iastb : iastbindings ) {
            if ( iastb != null && iastb instanceof IASTbinding ) {
                b.add((IASTbinding) iastb);
            } else {
                String msg = "Not an IASTbinding " + iastb;
                throw new ParseException(msg);
            }
        }
        IASTbinding[] bindings = b.toArray(new IASTbinding[0]);
        IASTexpression[] iasts = findThenParseChildAsExpressions(e, "body");
        IASTsequence body = getFactory().newSequence(iasts);
        return getFactory().newBlock(bindings, body);
    }
    
    public IASTbinding binding (Element e) throws ParseException {
        // Casts ensured by grammar9:
        Element v = findChild(e, "variable");
        String name = v.getAttribute("name");
        IASTvariable variable = getFactory().newVariable(name);
        IASTexpression exp = narrowToIASTexpression(
                findThenParseChildContent(e, "initialisation"));
        return getFactory().newBinding(variable, exp);
    }

    public IASTinvocation invocation (Element e) throws ParseException {
        // Casts ensured by grammar9:
        IASTexpression fun = narrowToIASTexpression(
                findThenParseChildContent(e, "function"));
        IASTexpression[] iasts = 
                findThenParseChildAsExpressions(e, "arguments");
        return getFactory().newInvocation(fun, iasts);
    }
    
    public IASTassignment assignment (Element e) throws ParseException {
        String name = e.getAttribute("name");
        IASTexpression value = narrowToIASTexpression(
                findThenParseChildContent(e, "value"));
        IASTvariable variable = getFactory().newVariable(name);
        return getFactory().newAssignment(variable, value);
    }
    
    public IASTloop loop (Element e) throws ParseException {
        IASTexpression condition = narrowToIASTexpression(
                findThenParseChildContent(e, "condition"));
        IASTexpression[] expressions = 
                findThenParseChildAsExpressions(e, "body");
        IASTexpression body = getFactory().newSequence(expressions);
        return getFactory().newLoop(condition, body);
    }
    
    public IASTfunctionDefinition functionDefinition (Element e) 
            throws ParseException {
        String name = e.getAttribute("name");
        List<IASTvariable> vs = new Vector<>();
        NodeList vars = findChild(e, "variables").getChildNodes();
        for ( int i=0 ; i<vars.getLength() ; i++ ) {
            Node nd = vars.item(i);
            if ( nd.getNodeType() == Node.ELEMENT_NODE ) {
                Element v = (Element) nd;
                String variableName = v.getAttribute("name");
                IASTvariable variable = getFactory().newVariable(variableName);
                vs.add(variable);
            }
        }
        IASTvariable[] variables = vs.toArray(new IASTvariable[0]);
        IASTexpression[] expressions =
                findThenParseChildAsExpressions(e, "body");
        IASTexpression body = getFactory().newSequence(expressions);
        return getFactory().newFunctionDefinition(name, variables, body);
    }
    
    public IASTtry tryInstruction (Element e) throws ParseException {
        IASTexpression[] expressions =
                findThenParseChildAsExpressions(e, "body");
        IASTexpression body = getFactory().newSequence(expressions);
        IASTlambda catcher = null;
        try {
            Element catchElement = findChild(e, "catch");
            String name = catchElement.getAttribute("exception");
            IASTvariable variable = getFactory().newVariable(name);
            IASTvariable[] variables = new IASTvariable[]{ variable };
            IASTexpression[] cexprs = 
                    findThenParseChildAsExpressions(e, "catch");
            IASTexpression cbody = getFactory().newSequence(cexprs);
            catcher = getFactory().newLambda(variables, cbody);
        } catch (ParseException exc) {
            // No catch element
        }
        IASTexpression finallyer = null;
        try {
            IASTexpression[] fexprs =
                    findThenParseChildAsExpressions(e, "finally");
            finallyer = getFactory().newSequence(fexprs);
        } catch (ParseException exc) {
            // No finally element
        }
        return getFactory().newTry(body, catcher, finallyer);
    }
    
    public IASTlambda lambda (Element e) throws ParseException {
        List<IASTvariable> vs = new Vector<>();
        NodeList vars = findChild(e, "variables").getChildNodes();
        for ( int i=0 ; i<vars.getLength() ; i++ ) {
            Node nd = vars.item(i);
            if ( nd.getNodeType() == Node.ELEMENT_NODE ) {
                Element v = (Element) nd;
                String variableName = v.getAttribute("name");
                IASTvariable variable = getFactory().newVariable(variableName);
                vs.add(variable);
            }
        }
        IASTvariable[] variables = vs.toArray(new IASTvariable[0]);
        IASTexpression[] expressions =
                findThenParseChildAsExpressions(e, "body");
        IASTexpression body = getFactory().newSequence(expressions);
        return getFactory().newLambda(variables, body);
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
