#include <iostream>

using namespace std;

int main() {
  int n;
  cin >> n;
  int num[n];
  for (int i = 0; i < n; i++)
    cin >> num[i];
  int c = 0;
  for (int i = 0; i < n; i++) {
    bool exist = false;
    for (int j = 0; j < n && !exist; j++)
      if (i != j) {
        int d = num[i] - num[j];
        if (d > 0)
          for (int k = 0; k < n && !exist; k++)
            if (k != j && num[k] == d) {
              c++;
              exist = true;
            }
      }
  }
  cout << c;
  return 0;
}
