#include <vector>

using namespace std;

class RemovedNumbers {
  public:
	  static vector<vector<long long>> removNb(long long n) {
      vector<vector<long long>> r;
      long long sum = (1 + n) * n / 2;
      for(long long x = 1; x <= n; x++) {
        if((sum - x) % (x + 1) == 0) {
          long long y = (sum - x) / (x + 1);
          if(y <= n) r.push_back({x, y});
        }
      }
      return r;
    }
};
