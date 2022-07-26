package misc;

import java.util.ArrayList;
import java.util.List;

public class PurityTester {

	public static void main(String[] args) {
		for (int i = -100; i < 101; i++) {
			Purity a = new Purity(i, 0);
			Purity b = new Purity(i, 5);
			Purity c = new Purity(i, 10);
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

	public static void basics(Purity i) {
		i.getValue();
		i.getShift();
		i.isHeavy();
		i.getNum();
		i.getJWrap();
	}
}
