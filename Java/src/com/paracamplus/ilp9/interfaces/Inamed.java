package com.paracamplus.ilp9.interfaces;

public interface Inamed extends IAST {
	String getName();
	
	default String getMangledName() {
	    final String s = getName();
        final StringBuffer sb = new StringBuffer(s.length());
        for ( int i=0 ; i<s.length() ; i++ ) {
          final char c = s.charAt(i);
          if (   ( 'a' <= c && c <= 'z' )
              || ( 'A' <= c && c <= 'Z' )
              || ( '0' <= c && c <= '9' )
              || ( '_' == c ) ) {
            sb.append(c);
          } else {
              sb.append("_");
              sb.append(Integer.toHexString(c));
          }
        }
        return sb.toString();
	}
}
