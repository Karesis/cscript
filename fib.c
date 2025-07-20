
int fib(int N) {
    int a = 0;
    int b = 1;
    int c = 0;
    while(N > 0) {
        c = b;
        b = a + b;
        a = c;
        N = N - 1;
    }
    return b;
}
int main() {
    return fib(10);
}