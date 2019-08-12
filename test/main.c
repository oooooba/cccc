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
int test17(int);
long test18(int, char);
int test19(void);
int test20(int);
int test21(void);
int test22(int, int);
int test23(int, int);
int test24(int, int);
int test25(int);
int test26(int);
int test27(int);
int test28(int, int);
int test29(int, int);
int test30(int);
int test31(int);
int test32(int);
int test33(int, int);
int test34(int, int);
int test35(int, int);
int test36(int, int);
int test37(int);
int test38(int);
int test39(int);
int test40(const char*);
int test41(const char*);
int test42(int);
int test43(void);
char test44(int x);
int test45(int, int);
int test46(int, int);
int test47(int);
int test48(int, int);
int test49(int, int);
int test50(void);
int test51(int);

static int num_test_counter = 0;
static int num_passed_test_counter = 0;

static void check(const char* msg, int expected, int actual) {
    ++num_test_counter;
    if (actual == expected) {
        ++num_passed_test_counter;
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
    check("test17", 0x78563412, test17(0x12345678));
    check("test18", 20, test18(4, 5));
    check("test19", 21, test19());
    check("test20", 9, test20(1));
    check("test21", 1, test21());
    check("test22", 1, test22(3, 3));
    check("test22", 0, test22(3, 4));
    check("test23", 0, test23(3, 3));
    check("test23", 1, test23(3, 4));
    check("test24", 0, test24(3, 3));
    check("test24", 1, test24(3, 4));
    check("test25", 2, test25(1));
    check("test26", 0, test26(1));
    check("test27", 55, test27(10));
    check("test28", 1, test28(1, 2));
    check("test28", 0, test28(2, 2));
    check("test28", 0, test28(2, 1));
    check("test29", 1, test29(1, 2));
    check("test29", 1, test29(2, 2));
    check("test29", 0, test29(2, 1));
    check("test30", 55, test30(10));
    check("test31", 55, test31(10));
    check("test32", 55, test32(10));
    check("test33", 0, test33(1, 2));
    check("test33", 0, test33(2, 2));
    check("test33", 1, test33(2, 1));
    check("test34", 0, test34(1, 2));
    check("test34", 1, test34(2, 2));
    check("test34", 1, test34(2, 1));
    check("test35", 15, test35(10, 5));
    check("test36", 100, test36(10, 5));
    check("test37", 0, test37(0));
    check("test37", 1, test37(1));
    check("test37", 2, test37(2));
    check("test38", 2, test38(0));
    check("test38", 2, test38(1));
    check("test38", 2, test38(2));
    check("test39", 0, test39(0));
    check("test39", 1, test39(1));
    check("test39", 2, test39(2));
    check("test40", 0, test40("cccc"));
    check("test41", 0, test41("cccc"));
    check("test42", 0, test42(0));
    check("test42", 1, test42(1));
    check("test42", 2, test42(2));
    check("test42", 3, test42(3));
    check("test42", 4, test42(4));
    check("test43", 0, test43());
    check("test44", 0x12, test44(0x12));
    check("test44", 0x34, test44(0x1234));
    check("test45", 6, test45(1, 5));
    check("test45", 4, test45(0, 5));
    check("test46", 6, test46(1, 5));
    check("test46", 4, test46(0, 5));
    check("test47", 1, test47(-1));
    check("test47", 2, test47(0));
    check("test47", 2, test47(1));
    check("test48", 1, test48(1, 1));
    check("test48", 0, test48(1, 0));
    check("test48", 0, test48(0, 1));
    check("test48", 0, test48(0, 0));
    check("test49", 1, test49(1, 1));
    check("test49", 1, test49(1, 0));
    check("test49", 1, test49(0, 1));
    check("test49", 0, test49(0, 0));
    check("test50", -1, test50());
    check("test51", -2, test51(2));
    check("test51", 2, test51(-2));

    bool failed = num_test_counter != num_passed_test_counter;
    if (failed) printf("\n\n============================================\n");
    printf("result: total = %d, passed = %d\n", num_test_counter,
           num_passed_test_counter);
    if (failed) printf("============================================\n\n\n");
    return 0;
}
