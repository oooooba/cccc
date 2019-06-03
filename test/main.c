#include <stdbool.h>
#include <stdio.h>

int test1(void);
int test2(void);
int test3(void);
int test4(void);
int test5(void);
int test6(void);
int test7(void);
int test8(void);
int test9(void);
int test10(void);
int test11(int);
int test12(int, int);
int test13(void);
int test14(void);
int test15(void);
int test16(void);

static void check(const char* msg, int expected, int actual) {
    if (actual == expected) {
        printf("%s: pass\n", msg);
    } else {
        printf("%s: fail (expected=%d, actual=%d)\n", msg, expected, actual);
    }
}

int main(void) {
    check("test1", 1, test1());
    check("test2", 3, test2());
    check("test3", 4, test3());
    check("test4", -5, test4());
    check("test5", -26, test5());
    check("test6", -5, test6());
    check("test7", -1, test7());
    check("test8", 30, test8());
    check("test9", 3, test9());
    check("test10", 4, test10());
    check("test11", 5, test11(2));
    check("test12", 4, test12(5, 1));
    check("test13", 1, test13());
    check("test14", 8, test14());
    check("test15", 1, test15());
    check("test16", 'B', test16());
    return 0;
}
