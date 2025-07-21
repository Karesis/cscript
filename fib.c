
int main() {
    return fib(10);
}
int fib(int N) {
    int a = 0;
    int b = 1;
    int c = 0;
    bool x = true;
    while(N > 0) {
        c = b;
        b = a + b;
        a = c;
        N = N - 1;
    }
    if(!x) {
        b + 10000;
    }
    return b;
}