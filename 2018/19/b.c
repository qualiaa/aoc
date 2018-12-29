#include <stdio.h>

int main() {
    //int target = 955;
    int target = 10551355;

    long long sum = 0;
    for (int i = 1; i <= target; ++i)
    {
        if (target % i == 0) {
            sum += i;
        }
    }

    printf("%d\n",sum);
}
