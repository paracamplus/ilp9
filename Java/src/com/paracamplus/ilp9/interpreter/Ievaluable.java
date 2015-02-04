package com.paracamplus.ilp9.interpreter;



public interface Ievaluable {
	Object eval(ILexicalEnvironment lexenv, ICommon common) 
	        throws EvaluationException;
}
