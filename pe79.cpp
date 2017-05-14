#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  string kl[50];
  int apps[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  for(int i = 0; i < 50; i++) {
    cin >> kl[i];
    apps[kl[i][0] - 48] = apps[kl[i][1] - 48] = apps[kl[i][2] - 48] = 10;
  }
  vector<int> r;
  for(int j = 0; j < 10; j++)
    if(apps[j] == 10) r.push_back(j);
  sort(r.begin(), r.end(), [&kl] (const int &a, const int &b) {
    for(int i = 0; i < 50; i++)
      if(b == kl[i][0] - 48 && (a == kl[i][1] - 48 || a == kl[i][2] - 48) ||
         b == kl[i][1] - 48 && a == kl[i][2] - 48) return false;
    return true;
  });
  for(vector<int>::iterator it = r.begin(); it != r.end(); it++)
    cout << *it;
  cout << endl;
}
