package com.paracamplus.ilp9.ast;

import com.paracamplus.ilp9.interfaces.Inamed;

public abstract class ASTnamed extends AST implements Inamed {
    
    public ASTnamed (String name) {
        this.name = name;
        this.mangledName = mangle(name);
    }
    private String name;
    private String mangledName;
    
    public String getName() {
        return name;
    }

    public String getMangledName() {
        return mangledName;
    }
    
    public static String mangle (final String s) {
        assert(s != null);
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
