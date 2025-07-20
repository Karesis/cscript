// test.c

int add(int a, int b) {
    return a + b;
}
int main() {
    int x = 10;
    int y = 32;
    while(x >= 0) {
        y = y - 1;
        x = x-1;
    }
    int* a = &x;
    int z = add(*a, y);
    return z;
}