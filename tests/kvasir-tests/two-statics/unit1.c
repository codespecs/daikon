static int static_function(int x) {
    return 42 + x;
}

int global_function1(int x) {
    return static_function(x);
}
