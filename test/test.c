int test1() { return 1; }

int test2() { return 1 + 2; }

int test3() { return 1 + 2 - 3 + 4; }

int test4() { return 1 + 2 * 3 * 4 - 5 * 6; }

int test5() {
    1 + 2 * 3;
    return 4 - 5 * 6;
}

int test6() {
    { return 1 + 2 * 3 * 4 - 5 * 6; }
    // comment
}
int test7() {
    if (1 + 2)
        return 3 - 4;
    else
        return 5 * 6;
}

int test8() {
    if (1 - 1)
        return 3 - 4;
    else
        return 5 * 6;
}

int test9() {
    int x;
    int y;
    y = 3;
    x = y;
    return x;
}

int test10() {
    int x;
    int *y;
    y = &x;
    *y = 4;
    return x;
}

int test11(int x) { return x + 3; }

int test12(int x, int y) { return x - y; }

int test13() { return test12(2 + 3, 4); }

int test14() { return 1 + test12(6, 4) + 5; }
