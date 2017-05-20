#include <vector>

using namespace std;

long long ceildiv(long long a, long long b) {
  return (a - 1) / b + 1;
}

class RemovedNumbers {
  public:
    static vector<vector<long long>> removNb(long long n) {
      vector<vector<long long>> r;
      long long sum = (1 + n) * n / 2;
      long long min = ceildiv(n * (n - 1), (2 * (n + 1)));
      for(long long x = min; x <= n; x++)
        if((sum - x) % (x + 1) == 0)
          r.push_back({x, (sum - x) / (x + 1)});
      return r;
    }
};
