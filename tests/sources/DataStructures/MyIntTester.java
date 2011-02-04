package DataStructures;

import java.util.*;

public class MyIntTester {

	public static void main(String[] args) {
		for (int i = -100; i < 101; i++) {
			MyInt a = new MyInt(i, 0);
			MyInt b = new MyInt(i, 5);
			MyInt c = new MyInt(i, 10);
			List<Number> l = new ArrayList<Number>();
			for (int j = 0; j < 20; j++) {
				l.add(new Integer(j));
			}
			basics(a);
			basics(b);
			basics(c);
			a.scale(i);
			b.scale(i);
			c.scale(i);
			a.sum(new Integer(i));
			b.sum((Number)new Integer(i));
			c.sum(new Integer(i));
			a.retrieve(l);
			b.retrieve(l);
			c.retrieve(l);
		}
	}
		
	public static void basics(MyInt i) {
		i.getValue();
		i.getShift();
		i.isHeavy();
		i.getNum();
		i.getJWrap();
	}
}
