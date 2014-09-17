#include <algorithm>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <vector>
#include <tuple>
#include <map>

using namespace std;

vector<int64_t> readNNumbers(int n)
{
    vector<int64_t> result;
    result.reserve(n);
    for(int i=0; i<n; ++i)
    {
        int64_t val;
        cin >> val;
        result.push_back(val);
    }
    return result;
}

int main()
{
    const int64_t limit = 1000000007;

    int n, m;
    cin >> n >> m;

    auto a = readNNumbers(n);
    auto b = readNNumbers(m);
    auto c = readNNumbers(m);

    map<int, int64_t> factors;

    for(int i = 0; i < m; ++i)
        if (factors.find(b[i]) == end(factors))
            factors[b[i]] = c[i];
        else
            factors[b[i]] = factors[b[i]] * c[i] % limit;

    for_each(begin(factors), end(factors),
        [&](pair<int, int64_t> keyAndVal)
    {
        int i;
        int64_t factor;
        std::tie(i, factor) = keyAndVal;
        for(int idx = i-1; idx < n; idx += i)
            a[idx] = a[idx] * factor % limit;
    });

    copy(begin(a), end(a), ostream_iterator<int64_t>(cout, " "));
}