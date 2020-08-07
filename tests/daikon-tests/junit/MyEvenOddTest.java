import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class MyEvenOddTest {

    @Test
    public void testEvenOddNumber() {
        MyEvenOdd meo = new MyEvenOdd();
        assertEquals("10 is a even number", true, meo.isEvenNumber(10));
    }
}
