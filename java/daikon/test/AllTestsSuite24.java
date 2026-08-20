package daikon.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Daikon's unit tests that require Java 24 or later. Does not include system tests.
 *
 * @see AllTestsSuite
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
  daikon.dcomp.CalcStackTest24.class,
  daikon.dcomp.DCInstrumentTest24.class,
  daikon.test.dcomp.DCInstrumentConcurrencyTest24.class,
  daikon.test.dcomp.TagFrameTest24.class,
})
public final class AllTestsSuite24 {}
