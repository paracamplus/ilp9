package com.paracamplus.ilp9.interpreter.primitive;

public class Print extends UnaryPrimitive {
    
    public Print() {
        super("print");
    }
        
    public Object apply (Object value) {
        System.out.print(value);
        return Boolean.FALSE;
    }
}
