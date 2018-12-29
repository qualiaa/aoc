#include <stdio.h>

int main() {
    //int target = 955;
    int target = 10551355;

    long long sum = 0;
    for (long long a = 1; a <= target; ++a)
    {
        if (target % a == 0) {
            sum += a;
        }
    }

    printf("%d\n",sum);
}
