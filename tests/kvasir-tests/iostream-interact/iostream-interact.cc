#include <iostream>

using namespace std;

void examine(int x, int y, int which) {}

int main() {
    int x = 10;
    int y = 20;

    examine(x, y, 1);
    cout << x << " " << y << endl;
    examine(x, y, 2);

    return 0;
}
    
