int test1(void) { return 1; }

int test2(void) { return 1 + 2; }

int test3(void) { return 1 + 2 - 3 + 4; }

int test4(void) { return 1 + 2 * 3 * 4 - 5 * 6; }

int test5(void) {
    1 + 2 * 3;
    return 4 - 5 * 6;
}

int test6(void) {
    { return 1 + 2 * 3 * 4 - 5 * 6; }
    // comment
}
int test7(void) {
    if (1 + 2)
        return 3 - 4;
    else
        return 5 * 6;
}

int test8(void) {
    if (1 - 1)
        return 3 - 4;
    else
        return 5 * 6;
}

int test9(void) {
    int x;
    int y;
    y = 3;
    x = y;
    return x;
}

int test10(void) {
    int x;
    int *y;
    y = &x;
    *y = 4;
    return x;
}

int test11(int x) { return x + 3; }

int test12(int x, int y) { return x - y; }

int test13(void) { return test12(2 + 3, 4); }

int test14(void) { return 1 + test12(6, 4) + 5; }

struct S {
    int x;
    int y;
};
struct T;

int test15(void) {
    struct S s;
    s.y = 1;
    s.x = 2;
    return s.y;
}

int test16(void) {
    char c;
    c = 65;
    return c + 1;
}
