package com.paracamplus.ilp9.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(value=Suite.class)
@SuiteClasses(value={
        com.paracamplus.ilp9.interpreter.test.ProcessTest.class,
        com.paracamplus.ilp9.compiler.test.ProcessTest.class,
})
public class WholeTestSuite {}
