package misc;

import java.util.*;

class Person {

    private String _name;
    private int _height;
    private int _weight;

    public Person(String name, int height, int weight) {
	_name = name;
	_height = height;
	_weight = weight;
    }

    public String getName() {
	return _name;
    }
}

public class EncapsulateDowncast {

    static final int CASES = 64;
    static final int SEED = 327;

    static Random random = new Random(SEED);
    static Random random2 = new Random(SEED);

    static void showLastPerson(Vector list) {
	    Person last = (Person)list.lastElement();
            // Reduce output, for test suite
	    // System.out.println("The last person in the list: " + last.getName());
    }

    public static void main(String argv[]) {
	for (int i = 0;
	     i < CASES;
	     i++) {
	    Vector list = new Vector();
	    for (int j = 0;
		 j <= random.nextInt(CASES);
		 j++) {
		String name = "name" + j;
		int height = random.nextInt(12);
		int weight = random2.nextInt(100) + 100;
		Person person = new Person(name, height, weight);
		list.addElement(person);
	    }
	    showLastPerson(list);
	}
    }
}
