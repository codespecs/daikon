#include <stdio.h>

class Shape {
public:
    virtual int num_sides() = 0;

    static bool compare(Shape *s1, Shape *s2);
};

bool Shape::compare(Shape *s1, Shape *s2) {
    return s1->num_sides() == s2->num_sides();
}

class Triangle : public Shape {
    virtual int num_sides();
};

class Square : public Shape {
    virtual int num_sides();
};

int Triangle::num_sides() {
    return 3;
}

int Square::num_sides() {
    return 4;
}

int main(int argc, char **argv) {
    Shape *shapes[10];
    for (int i = 0; i < 10; i++) {
	shapes[i] = (i & 1) ? (Shape*) new Triangle() : (Shape *) new Square();
    }
    for (int i = 0; i < 10; i++) {
	printf("Shape %d has %d sides\n", i, shapes[i]->num_sides());
    }
    Shape::compare(shapes[0], shapes[1]);
    Shape::compare(shapes[2], shapes[4]);
}
