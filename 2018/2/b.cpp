#include <deque>
#include <functional>
#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

using Input = std::istream_iterator<std::string>;

int main() {
    std::vector<std::string> strings;
    std::copy(Input(std::cin), Input(), std::back_inserter(strings));

    for (auto it = strings.begin(); it != strings.end(); ++it) {
        auto const& s = *it;
        std::deque<bool> diffs(s.length());
        if (strings.end() != std::find_if(it+1, strings.end(),
                [&s,&diffs] (auto const& t) {
                    std::transform(s.begin(), s.end(), t.begin(),
                            diffs.begin(), std::minus());
                    return std::accumulate(diffs.begin(), diffs.end(), 0) <= 1;
                })) {
            for (int i = 0; i < s.length(); ++i)
                if (not diffs[i]) std::cout << s[i];
            std::cout << '\n';
        }
    }
}
