// ffi_test.csc

extern "C" {
    printf(format: char*, ...) -> i32;
}

main() -> i32 {
    num: i32 = 42;
    printf("Hello from CScript! The magic number is %d\n", num);
    return 0;
}