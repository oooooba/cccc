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
    int* y;
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

int test17(int i) {
    char* c = (char*)&i;
    char tmp;
    tmp = c[0];
    c[0] = c[3];
    c[3] = tmp;
    tmp = c[2];
    c[2] = c[1];
    c[1] = tmp;
    return i;
}

long test(char c, int i);

long test18(int i, char c) { return test(c, i); }

long test(char c, int i) { return c * i; }

int test19(void) {
    return sizeof(int) + sizeof(char) + sizeof(long) + sizeof(int*);
}

typedef int Int;
typedef Int Int2;
typedef struct S TS;

Int test20(Int2 n) {
    TS ts;
    if (sizeof(Int) - sizeof(Int2)) {
        ts.x = 0;
        ts.y = 0;
    } else {
        ts.x = n;
        ts.y = sizeof(TS);
    }
    return ts.x + ts.y;
}

int test21(void) {
    struct S s;
    s.y = 1;
    s.x = 2;
    struct S* sp = &s;
    sp->x = sp->y;
    return sp->x;
}

int test22(int x, int y) { return x == y; }

int test23(int x, int y) { return !test22(x, y); }

int test24(int x, int y) { return x != y; }

int test25(int x) {
    ++x;
    return x;
}

int test26(int x) {
    --x;
    return x;
}

int test27(int x) {
    int sum = 0;
    while (x) {
        sum = sum + x;
        --x;
    }
    return sum;
}

int test28(int x, int y) { return x < y; }

int test29(int x, int y) { return x <= y; }

int test30(int x) {
    int sum = 0;
    for (int i = 1; i <= x; ++i) {
        sum = sum + i;
    }
    return sum;
}

int test31(int x) {
    int sum = 0;
    int i;
    for (i = 1; i <= x; ++i) {
        sum = sum + i;
    }
    return sum;
}

int test32(int x) {
    int sum = 0;
    int i = 1;
    for (; i <= x; ++i) {
        sum = sum + i;
    }
    return sum;
}

int test33(int x, int y) { return x > y; }

int test34(int x, int y) { return x >= y; }

int test35(int x, int y) {
    int sum = 0;
    for (int i = 1; i <= x; ++i) {
        if (i > y) break;
        sum = sum + i;
    }
    return sum;
}

int test36(int x, int y) {
    int sum = 0;
    for (int i = 0; i < x; ++i) {
        for (int j = 1; j <= y; ++j) {
            if (j > y - 1) break;
            sum = sum + j;
        }
    }
    return sum;
}

int test37(int x) {
    int result;
    switch (x) {
        case 0:
            result = 0;
            break;
        case 1:
            result = 1;
            break;
        default:
            result = 2;
    }
    return result;
}

int test38(int x) {
    int result;
    switch (x) {
        case 0:
            result = 0;
        case 1:
            result = 1;
        default:
            result = 2;
    }
    return result;
}

enum E {
    E_0,
    E_1,
};

int test39(enum E x) {
    int result;
    switch (x) {
        case E_0:
            result = 0;
            break;
        case E_1:
            result = 1;
            break;
        default:
            result = 2;
    }
    return result;
}

#include <string.h>

int test40(const char* s) { return strcmp(s, "cccc"); }

#include <stdio.h>

int test41(const char* s) {
    printf("[[%s]]\n", s);
    return 0;
}

struct SI {
    int s1;
    struct {
        int s2;
        char s3;
    };
    int s4;
};

int test42(int i) {
    struct SI si;
    si.s1 = 1;
    si.s2 = 2;
    si.s3 = 3;
    si.s4 = 4;
    if (i == 1) {
        return si.s1;
    } else if (i == 2) {
        return si.s2;
    } else if (i == 3) {
        return si.s3;
    } else if (i == 4) {
        return si.s4;
    } else {
        return 0;
    }
}
