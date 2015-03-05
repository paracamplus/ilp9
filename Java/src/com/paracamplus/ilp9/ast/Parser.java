/* *****************************************************************
 * ILP9 - Implantation d'un langage de programmation.
 * by Christian.Queinnec@paracamplus.com
 * See http://mooc.paracamplus.com/ilp9
 * GPL version 3
 ***************************************************************** */
package com.paracamplus.ilp9.ast;

import java.io.File;
import java.io.StringReader;
import java.util.List;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.paracamplus.ilp9.interfaces.IAST;
import com.paracamplus.ilp9.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp9.interfaces.IASTclassDefinition;
import com.paracamplus.ilp9.interfaces.IASTexpression;
import com.paracamplus.ilp9.interfaces.IASTfunctionDefinition;
import com.paracamplus.ilp9.interfaces.IASTlambda;
import com.paracamplus.ilp9.interfaces.IASTmethodDefinition;
import com.paracamplus.ilp9.interfaces.IASTnamedLambda;
import com.paracamplus.ilp9.interfaces.IASToperator;
import com.paracamplus.ilp9.interfaces.IASTprogram;
import com.paracamplus.ilp9.interfaces.IASTvariable;
import com.paracamplus.ilp9.parser.AbstractExtensibleParser;
import com.paracamplus.ilp9.parser.IParserFactory;
import com.paracamplus.ilp9.parser.ParseException;
import com.paracamplus.ilp9.tools.Input;
import com.thaiopensource.validate.ValidationDriver;

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
        addMethod("codefinitions", Parser.class);
        addMethod("classDefinition", Parser.class);
        addMethod("instantiation", Parser.class);
        addMethod("fieldRead", Parser.class);
        addMethod("fieldWrite", Parser.class);
        addMethod("send", Parser.class);
        addMethod("self", Parser.class);
        addMethod("superInvocation", Parser.class, "super");
	}
	  
    public void setInput(Input input) {
        this.input = input;
    }
    private Input input;
    
    public void setGrammar (File rngFile) {
        this.rngFile = rngFile;
    }
    private File rngFile;
    
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
            IASTprogram program = parse(document);
            return program;
        } catch (ParseException e) {
            throw e;
        } catch (Exception e) {
            throw new ParseException(e);
        }
    }   

    // 
    
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
        IASTexpression body = getFactory().newSequence(exprs);
        return getFactory().newProgram(defs, clazzes, body);
    }

	public IASTexpression alternative (Element e) throws ParseException {
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
	
	public IASTexpression sequence (Element e) throws ParseException {
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
	
	public IASTexpression integerConstant (Element e) throws ParseException {
	    final String description = e.getAttribute("value");
	    return getFactory().newIntegerConstant(description);
	}

    public IASTexpression floatConstant (Element e) throws ParseException {
        final String description = e.getAttribute("value");
        return getFactory().newFloatConstant(description);
    }

    public IASTexpression stringConstant (Element e) throws ParseException {
        final String description = e.getTextContent();
        return getFactory().newStringConstant(description);
    }

    public IASTexpression booleanConstant (Element e) throws ParseException {
        final String description = e.getAttribute("value");
        return getFactory().newBooleanConstant(description);
    }
    
    public IASTexpression variable (Element e) throws ParseException {
        final String name = e.getAttribute("name");
        IASTvariable variable = getFactory().newVariable(name);
        return variable;
    }
    
    public IASTexpression unaryOperation (Element e) 
            throws ParseException {
        IAST iast = findThenParseChildContent(e, "operand");
        IASTexpression operand = narrowToIASTexpression(iast);
        String operatorName = e.getAttribute("operator");
        IASToperator operator = getFactory().newOperator(operatorName);
        return getFactory().newUnaryOperation(operator, operand);
    }

    public IASTexpression binaryOperation (Element e) 
            throws ParseException {
        IAST iast1 = findThenParseChildContent(e, "leftOperand");
        IASTexpression operand1 = narrowToIASTexpression(iast1);
        IAST iast2 = findThenParseChildContent(e, "rightOperand");
        IASTexpression operand2 = narrowToIASTexpression(iast2);
        String operatorName = e.getAttribute("operator");
        IASToperator operator = getFactory().newOperator(operatorName);
        return getFactory().newBinaryOperation(operator, operand1, operand2);
    }
    
    public IASTexpression block (Element e) throws ParseException {
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
        IASTexpression body = getFactory().newSequence(iasts);
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

    public IASTexpression invocation (Element e) throws ParseException {
        // Casts ensured by grammar9:
        IASTexpression fun = narrowToIASTexpression(
                findThenParseChildContent(e, "function"));
        IASTexpression[] iasts = 
                findThenParseChildAsExpressions(e, "arguments");
        return getFactory().newInvocation(fun, iasts);
    }
    
    public IASTexpression assignment (Element e) throws ParseException {
        String name = e.getAttribute("name");
        IASTexpression value = narrowToIASTexpression(
                findThenParseChildContent(e, "value"));
        IASTvariable variable = getFactory().newVariable(name);
        return getFactory().newAssignment(variable, value);
    }
    
    public IASTexpression loop (Element e) throws ParseException {
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
        IASTvariable functionVariable = getFactory().newVariable(name);
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
        return getFactory().newFunctionDefinition(
                functionVariable, variables, body);
    }
    
    public IASTexpression tryInstruction (Element e) throws ParseException {
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
    
    public IASTexpression codefinitions (Element e) throws ParseException {
        List<IASTnamedLambda> fs = new Vector<>();
        for ( IAST ifd : findThenParseChildAsArray(e, "functions")) {
            // cast ensured by grammar9
            IASTfunctionDefinition fd = (IASTfunctionDefinition) ifd;
            IASTnamedLambda fun = getFactory().newNamedLambda(
                    fd.getFunctionVariable(),
                    fd.getVariables(),
                    fd.getBody());
            fs.add(fun);
        }
        IASTexpression[] expressions =
                findThenParseChildAsExpressions(e, "body");
        IASTexpression body = getFactory().newSequence(expressions);
        return getFactory().newCodefinitions(
                fs.toArray(new IASTnamedLambda[0]),
                body );
    }
    
    public IASTclassDefinition classDefinition (Element e) 
            throws ParseException {
        String className = e.getAttribute("name");
        String superClassName = e.getAttribute("parent");

        try {
            final XPathExpression fieldPath =
                xPath.compile("./fields/field");
            final NodeList nlFields = (NodeList)
                fieldPath.evaluate(e, XPathConstants.NODESET);
            final List<String> fieldNames = new Vector<>();
            for ( int i=0 ; i<nlFields.getLength() ; i++ ) {
                final Element n = (Element) nlFields.item(i);
                fieldNames.add(n.getAttribute("name"));
            }

            final XPathExpression methodPath =
                xPath.compile("./methods/method");
            final NodeList nlMethods = (NodeList)
                methodPath.evaluate(e, XPathConstants.NODESET);
            final List<IASTmethodDefinition> methodDefinitions = new Vector<>();
            for ( int i=0 ; i<nlMethods.getLength() ; i++ ) {
                final Element method = (Element) nlMethods.item(i);
                final IASTmethodDefinition m = 
                        methodDefinition(method, className);
                methodDefinitions.add(m);
            }
            return getFactory().newClassDefinition(
                    className, 
                    superClassName, 
                    fieldNames.toArray(new String[0]), 
                    methodDefinitions.toArray(new IASTmethodDefinition[0]));
        } catch (XPathExpressionException e1) {
            throw new ParseException(e1);
        }
    }
    private static final XPath xPath = XPathFactory.newInstance().newXPath();
    
    public IASTmethodDefinition methodDefinition (
            Element e, 
            String definingClassName)
            throws ParseException {
        String name = e.getAttribute("name");
        IASTvariable methodVariable = getFactory().newVariable(name);
        List<IASTvariable> vs = new Vector<>();
        vs.add(getFactory().newSelf());
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
        return getFactory().newMethodDefinition(
                methodVariable, variables, body, 
                name, definingClassName);
    }
    
    public IASTexpression instantiation (Element e)
            throws ParseException {
        String className = e.getAttribute("class");
        final IAST[] iasts = parseAll(e.getChildNodes());
        List<IASTexpression> exprs = new Vector<>();
        for ( IAST iast : iasts ) {
            if ( iast != null && iast instanceof IASTexpression ) {
                exprs.add((IASTexpression) iast);
            }
        }
        IASTexpression[] arguments = exprs.toArray(new IASTexpression[0]);
        return getFactory().newInstantiation(className, arguments);
    }
    
    public IASTexpression fieldRead (Element e)
            throws ParseException {
        String fieldName = e.getAttribute("field");
        IASTexpression target = narrowToIASTexpression(
                findThenParseChildContent(e, "target"));
        return getFactory().newReadField(fieldName, target);
    }
    
    public IASTexpression fieldWrite (Element e)
            throws ParseException {
        String fieldName = e.getAttribute("field");
        IASTexpression target = narrowToIASTexpression(
                findThenParseChildContent(e, "target"));
        IASTexpression value = narrowToIASTexpression(
                findThenParseChildContent(e, "value"));
        return getFactory().newWriteField(fieldName, target, value);
    
    }
    
    public IASTexpression send (Element e)
            throws ParseException {
        String messageName = e.getAttribute("message");
        IASTexpression receiver = narrowToIASTexpression(
                findThenParseChildContent(e, "receiver"));
        final IASTexpression[] arguments = findThenParseChildAsExpressions(e, "arguments");
        return getFactory().newSend(messageName, receiver, arguments);
    }
    
    public IASTvariable self (Element e)
            throws ParseException {
        return getFactory().newSelf();
    }
    
    public IASTexpression superInvocation (Element e)
            throws ParseException {
        return getFactory().newSuper();
    }
}
