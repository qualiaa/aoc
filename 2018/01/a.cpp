#include <iostream>
#include <iterator>
#include <numeric>

using It = std::istream_iterator<int>;

int main() {
    std::cout << std::accumulate(It(std::cin), It(), 0) << '\n'; 
}
