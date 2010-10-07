static int static_function(int x) {
    return 69 + x;
}

int global_function2(int x) {
    return static_function(x);
}
