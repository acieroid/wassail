#include <stdio.h>

// Global variables
int global_counter = 0;
int global_sum = 0;

// (func0)
int func0(int n) {
    int x = n + 1;
    int y = n * 2;
    int z = n / 3;

    if (n <= 0) return 0;
    return x + y + z; //+ func0(n - 1);
}

// Non-recursive function (func1)
int func1(int a) {
    int p = a + 1;
    int q = a + 2;
    int r = a + 3;

    global_counter += 1;
    return p * q + r;
}

// Another non-recursive function (func2)
int func2(int b) {
    int m = b - 1;
    int n = b - 2;
    int o = b - 3;

    global_sum += m + n + o;
    return global_sum;
}

// Main function
int main() {
    int result0 = func0(3);
    int result1 = func1(5);
    int result2 = func2(10);

    printf("func0 result: %d\n", result0);
    printf("func1 result: %d\n", result1);
    printf("func2 result: %d\n", result2);

    printf("global_counter: %d\n", global_counter);
    printf("global_sum: %d\n", global_sum);

    return 0;
}