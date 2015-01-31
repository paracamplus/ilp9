package com.paracamplus.ilp9.parser;

@SuppressWarnings("serial")
public class ParseException extends Exception {
	public ParseException (final Throwable cause) {
		super(cause);
	}

	public ParseException (final String message) {
		super(message);
	}
}
