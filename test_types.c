/*
 * CScript Rich Type System Demonstration
 *
 * This program tests the newly integrated rich type system, including:
 * - Signed and unsigned integers of different bit widths (i16, u8).
 * - 64-bit floating-point numbers (f64).
 * - Type-specific operations (float arithmetic, unsigned integer wraparound).
 * - Pointers to new types (u8*).
 * - All code is written in the new 'modern' CScript syntax.
 */

// --- Global Variables ---
const PI: f64 = 3.14159;

// --- Helper Functions ---

// Increments the value pointed to by a u8 pointer.
// This will correctly demonstrate unsigned integer wraparound.
increment_u8_ptr(val_ptr: u8*) -> void {
    *val_ptr = *val_ptr + 1;
}

// Calculates the area of a circle.
// This tests f64 parameters, return value, and arithmetic.
calculate_circle_area(radius: f64) -> f64 {
    return PI * radius * radius;
}

// --- Main Program Entry Point ---
main() -> int {
    // Initialize a result variable. We will add to this based on test outcomes.
    result: i32 = 0;

    // --- Test 1: Floating-Point Arithmetic and Comparisons ---
    radius: f64 = 10.0;
    area: f64 = calculate_circle_area(radius); // area should be ~314.159

    // Check if the float calculation is within an expected range.
    if (area > 314.0 && area < 315.0) {
        // If the test passes, add 100 to the result.
        result = result + 100;
    }

    // --- Test 2: Unsigned Integer Wraparound ---
    // u8 can hold values from 0 to 255.
    counter: u8 = 254;
    increment_u8_ptr(&counter); // counter is now 255
    increment_u8_ptr(&counter); // counter should wrap around to 0

    // Check if the wraparound was successful.
    if (counter == 0) {
        // If the test passes, add 20 to the result.
        result = result + 20;
    }

    // --- Test 3: Signed Integer Comparisons ---
    neg_val: i16 = -10;
    pos_val: i16 = 10;

    // Check if signed comparison works correctly.
    if (neg_val < pos_val) {
        // If the test passes, add 3 to the result.
        result = result + 3;
    }

    // The final return value should be the sum of all successful test outcomes.
    // Expected: 100 (from float test) + 20 (from u8 test) + 3 (from i16 test) = 123
    return result;
}
