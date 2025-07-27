/*
 * CScript Struct Feature Comprehensive Test
 *
 * This program is the ultimate test for CScript's struct capabilities, including:
 * 1. Struct definition (Rust-style).
 * 2. Struct instantiation using aggregate literals (C-style).
 * 3. Member access (read and write).
 * 4. Passing structs to functions by value.
 * 5. Passing structs to functions by pointer to modify them.
 * All code is written in the modern CScript syntax.
 */

// --- Struct Definition ---
struct Point {
    x: i32,
    y: i32
}

// --- Helper Functions ---

// Takes a Point by value, calculates the sum of its members,
// and returns it. This tests passing structs by value.
sum_point_members(p: Point) -> i32 {
    return p.x + p.y;
}

// Takes a pointer to a Point and moves it diagonally.
// This tests passing structs by pointer and modifying members.
move_point(p_ptr: Point*) -> void {
    (*p_ptr).x = (*p_ptr).x + 10;
    (*p_ptr).y = (*p_ptr).y - 5;
}

// --- Main Program Entry Point ---
main() -> i32 {
    // --- Test 1: Instantiation and Member Read ---
    // Instantiate a Point using the new aggregate literal syntax.
    // The type is determined by the context `my_point: Point`.
    my_point: Point = { 10, 20 };

    // The initial sum should be 10 + 20 = 30.
    initial_sum: i32 = sum_point_members(my_point);

    // --- Test 2: Member Write and Pointer Modification ---
    // Directly modify a member.
    my_point.x = my_point.x + 2; // my_point is now { 12, 20 }

    // Pass a pointer to the struct to modify it.
    move_point(&my_point); // my_point should become { 12 + 10, 20 - 5 } = { 22, 15 }

    // --- Test 3: Verification ---
    // We will build the final return code based on the results.
    result: i32 = 0;

    // Check if the initial sum was correct.
    if (initial_sum == 30) {
        result = result + 100;
    }

    // Check if the final x coordinate is correct.
    if (my_point.x == 22) {
        result = result + 30;
    }

    // Check if the final y coordinate is correct.
    if (my_point.y == 15) {
        result = result + 7;
    }

    // The final return value should be the sum of all successful test outcomes.
    // Expected: 100 (from initial sum) + 30 (from final x) + 7 (from final y) = 137
    return result;
}
