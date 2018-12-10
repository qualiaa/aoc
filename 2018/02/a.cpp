#include <algorithm>
#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

using Input = std::istream_iterator<std::string>;

struct Counter {
    int two, three;
    Counter(std::string const& s) : two{}, three{} {
        for (char c : s)  {
            int n = std::count(s.begin(), s.end(), c);
            if (n == 2) two   = 1;
            if (n == 3) three = 1;
            if (two and three) break;
        }
    }
    Counter& operator+(Counter const& o) {
        two   += o.two; three += o.three; return *this;
    }
    operator int() { return two * three; }
};

int main()
{
    std::vector<Counter> counts;
    std::copy(Input(std::cin), Input(), std::back_inserter(counts));
    int result = std::accumulate(counts.begin(), counts.end()-1, counts.back());
    std::cout << result << '\n';
}
