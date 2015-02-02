package com.paracamplus.ilp9.parser;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.paracamplus.ilp9.interfaces.IAST;

public abstract class AbstractExtensibleParser extends AbstractParser {

	public AbstractExtensibleParser(IParserFactory factory) {
		super(factory);
        this.parsers = new HashMap<>();
	}
    private final HashMap<String, Method> parsers;

    public void addParser (String name, Method method) {
        this.parsers.put(name, method);
    }

    public void addMethod (String name, Class<?> clazz) {
        try {
          for ( Method m : clazz.getMethods() ) {
              if ( ! name.equals(m.getName()) ) {
                  continue;
              }
              if ( Modifier.isStatic(m.getModifiers()) ) {
                  continue;
              }
              Class<?>[] parameterTypes = m.getParameterTypes();
              if ( parameterTypes.length != 1 ) {
                  continue;
              }
              if ( ! Element.class.isAssignableFrom(parameterTypes[0]) ) {
                  continue;
              }
              addParser(name, m);
              return;
          }
          if ( Object.class == clazz ) {
              final String msg = "Cannot find suitable parsing method!";
              throw new RuntimeException(msg);
          } else {
              addMethod(name, clazz.getSuperclass());
          }
        } catch (SecurityException e1) {
          final String msg = "Cannot access parsing method!";
          throw new RuntimeException(msg);
        }
    }
    
    public IAST parse (final Node n) throws ParseException {
      switch ( n.getNodeType() ) {
      case Node.ELEMENT_NODE: {
        final Element e = (Element) n;
        final String name = e.getTagName();

        if ( parsers.containsKey(name) ) {
            final Method method = parsers.get(name);
            try {
              Object result = method.invoke(this, new Object[]{e});
              if ( result instanceof IAST ) {
            	  return (IAST) result;
              } else {
            	  final String msg = "Not an IAST " + result;
            	  throw new ParseException(msg);
              }
            } catch (IllegalArgumentException exc) {
              throw new ParseException(exc);
            } catch (IllegalAccessException exc) {
              throw new ParseException(exc);
            } catch (InvocationTargetException exc) {
                Throwable t = exc.getTargetException();
                if ( t instanceof ParseException ) {
                    throw (ParseException) t;
                } else {
                    throw new ParseException(exc);
                }
            }

        } else {
          final String msg = "Unknown element name: " + name;
          throw new ParseException(msg);
        }
      }

      default: {
        final String msg = "Unknown node type: " + n.getNodeName();
        throw new ParseException(msg);
      }
      }

    }
}
