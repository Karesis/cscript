/*
 * CScript Comprehensive Demonstration
 *
 * This program tests a variety of CScript features including:
 * - Global variables (const and mutable)
 * - Function definitions (including recursion and pointers)
 * - Control flow (if/else, while)
 * - Pointer manipulation (&, *)
 * - Scoping rules
 */

// --- Global Variables ---
const int TARGET_FACTORIAL = 5;
int SWAP_COUNTER = 0; // A global counter to track how many times swap is called.

// --- Function Declarations ---

// A function to swap two integers using pointers.
// It also increments a global counter.
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
    SWAP_COUNTER = SWAP_COUNTER + 1;
}

// A recursive function to calculate the factorial of a number.
int factorial(int n) {
    if (n < 2) {
        return 1;
    }
    return n * factorial(n - 1);
}

// --- Main Program Entry Point ---
int main() {
    int x = 10;
    int y = TARGET_FACTORIAL; // y is initially 5

    // Print initial state (conceptually, we can't print yet)
    // Expected: x = 10, y = 5, SWAP_COUNTER = 0

    // Call swap to test pointer functionality and global variable modification.
    swap(&x, &y);

    // After swap, values should be exchanged.
    // Expected: x = 5, y = 10, SWAP_COUNTER = 1

    int result = 0;

    // Use an if/else block to decide the calculation.
    // The condition (SWAP_COUNTER == 1 && y == 10) should be true.
    if (SWAP_COUNTER == 1 && y > x) {
        // This block should be executed.
        // Calculate factorial of the smaller number, which is now in 'x'.
        int fact_res = factorial(x); // factorial(5) = 120

        // Use a nested block to test scoping.
        {
            int inner_var = 10;
            result = fact_res + inner_var; // result = 120 + 10 = 130
        }
        // 'inner_var' is now out of scope.

    } else {
        // This block should NOT be executed.
        // If it were, it would return a simple error code.
        return 99; 
    }

    // Use a while loop to demonstrate loop functionality.
    // This loop will subtract the final swapped value of 'y' (which is 10)
    // from the result.
    int i = 0;
    while (i < y) { // Loop 10 times
        result = result - 1;
        i = i + 1;
    }
    // After loop: result = 130 - 10 = 120.

    // The final return value should be 120.
    return result;
}
