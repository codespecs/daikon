package misc;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Purity {
	private int value;
	private int shift;
	private boolean heavy;
	private LinkedList<Number> list1;
	private List<Number> list2;

	public Purity(int value, int shift) {
		this.value = value;
		this.shift = shift;
		this.heavy = shift >= 5;
		list1 = new LinkedList<Number>();
		list2 = new ArrayList<Number>();
		for (int j = 0; j < 11; j++) {
			list1.add(new Integer(j));
			list2.add(new Integer(j));
		}
	}

	public int getValue() {
		return value;
	}

	public int getShift() {
		return shift;
	}

	public boolean isHeavy() {
		return heavy;
	}

	public Number getNum() {
		return (Number) new Integer(value);
	}

	public Integer getJWrap() {
		return new Integer(value);
	}

	public int scale(int scale) {
		return value + shift * scale;
	}

	public int sum(Number n) {
		return value + n.intValue();
	}

	public int retrieve(List<Number> l) {
		return l.get(shift).intValue();
	}
}
