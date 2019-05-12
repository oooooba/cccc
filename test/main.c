#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

intptr_t test1(void);
intptr_t test2(void);
intptr_t test3(void);
intptr_t test4(void);
intptr_t test5(void);
intptr_t test6(void);
intptr_t test7(void);
intptr_t test8(void);
intptr_t test9(void);
intptr_t test10(void);
intptr_t test11(intptr_t);
intptr_t test12(intptr_t, intptr_t);
intptr_t test13(void);
intptr_t test14(void);

static void check(const char* msg, intptr_t expected, intptr_t actual) {
    if (actual == expected) {
        printf("%s: pass\n", msg);
    } else {
        printf("%s: fail (expected=%ld, actual=%ld)\n", msg, expected, actual);
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
    return 0;
}
