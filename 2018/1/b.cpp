#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

using Input = std::istream_iterator<int>;

int main() {
    std::vector<int> df;
    std::vector<int> f{0};
    std::copy(Input(std::cin), Input(), std::back_inserter(df));

    int sum = 0;
    while (df.end() == std::find_if(df.begin(), df.end(), [&] (int x) {
                    sum += x;
                    if (std::find(f.begin(), f.end(), sum) != f.end()) {
                        return true;
                    }
                    f.push_back(sum);
                    return false;
                }));
    std::cout << sum << '\n';
}
