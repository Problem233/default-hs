#include <iostream>
#include <algorithm>

using namespace std;

struct Farmer {
  int p;
  int a;
};

int main() {
  int n, m;
  cin >> n >> m;
  Farmer f[m];
  for(int i = 0; i < m; i ++)
    cin >> f[i].p >> f[i].a;
  sort(f, f + m, [](Farmer f1, Farmer f2) {
    return (f1.p < f2.p);
  });
  int v = 0;
  for(int i = 0; n > 0; i ++)
    if(n >= f[i].a) {
      v += f[i].p * f[i].a;
      n -= f[i].a;
    } else {
      v += f[i].p * n;
      n = 0;
    }
  cout << v << endl;
  return 0;
}
