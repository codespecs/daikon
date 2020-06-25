package MapQuick;

import java.util.NoSuchElementException;
import junit.framework.*;
import org.junit.Test;

public class PriorityQueueTest extends TestCase {

  public PriorityQueueTest(String name) {
    super(name);
  }

  @Test
  public void testSize() {
    PriorityQueue q = new PriorityQueue();
    assertEquals(q.size(), 0);
    for (int i = 1; i < 10; i++) {
      q.insert(i, new Object());
      assertEquals(q.size(), i);
    }

    q = new PriorityQueue();
    assertEquals(q.size(), 0);
    for (int i = 1; i < 10; i++) {
      q.insert(100 - i, new Object());
      assertEquals(q.size(), i);
    }
  }

  private void assertContains(PriorityQueue q, Object o) {
    assertTrue(q.contains(o));
  }

  private void assertNotContains(PriorityQueue q, Object o) {
    assertTrue(!q.contains(o));
  }

  private void assertContainsMatched(PriorityQueue q, Object[] o, boolean[] contains) {
    for (int i = o.length; i < o.length; i++) {
      assertTrue(q.contains(o[i]) == contains[i]);
    }
  }

  private static void insert(PriorityQueue q, Double d) {
    q.insert(d.doubleValue(), d);
  }

  @Test
  public void testContains() {
    PriorityQueue q = new PriorityQueue();
    Double[] d = new Double[5];
    boolean[] in = new boolean[d.length];
    for (int i = 0; i < d.length; i++) {
      d[i] = new Double(i);
      in[i] = false;
    }

    assertContainsMatched(q, d, in);
    insert(q, d[0]);
    in[0] = true;
    assertContainsMatched(q, d, in);
    insert(q, d[2]);
    in[2] = true;
    assertContainsMatched(q, d, in);
    insert(q, d[4]);
    in[4] = true;
    assertContainsMatched(q, d, in);
    q.extractMin();
    in[0] = false;
    assertContainsMatched(q, d, in);
    q.extractMin();
    in[2] = false;
    assertContainsMatched(q, d, in);
    q.extractMin();
    in[4] = false;
    assertContainsMatched(q, d, in);
  }

  @Test
  public void testExtract() {
    PriorityQueue q = new PriorityQueue();
    Double[] d = new Double[5];
    boolean[] in = new boolean[d.length];
    for (int i = 0; i < d.length; i++) {
      d[i] = new Double(i);
      in[i] = false;
    }

    insert(q, d[0]);
    in[0] = true;
    insert(q, d[4]);
    in[4] = true;
    insert(q, d[2]);
    in[2] = true;
    insert(q, d[3]);
    in[3] = true;
    insert(q, d[1]);
    in[1] = true;
    assertContainsMatched(q, d, in);
    assertEquals(q.extractMin(), d[0]);
    in[0] = false;
    assertContainsMatched(q, d, in);
    assertEquals(q.extractMin(), d[1]);
    in[1] = false;
    assertContainsMatched(q, d, in);
    assertEquals(q.extractMin(), d[2]);
    in[2] = false;
    assertContainsMatched(q, d, in);
    assertEquals(q.extractMin(), d[3]);
    in[3] = false;
    assertContainsMatched(q, d, in);
    assertEquals(q.extractMin(), d[4]);
    in[4] = false;
    assertContainsMatched(q, d, in);
    try {
      q.extractMin();
      fail("Expected exception on empty extract");
    } catch (NoSuchElementException e) {
    }
  }

  @Test
  public void testInsert() {
    // Most already covered by testContains
    PriorityQueue q = new PriorityQueue();

    Object o = new Object();
    q.insert(0, o);
    try {
      q.insert(1, o);
      fail("Expected exception on duplicate insert");
    } catch (PriorityQueue.DuplicateElementException e) {
    }
  }
}
