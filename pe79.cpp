#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  string kl[50];
  char apps[] = "0123456789";
  for(int i = 0; i < 50; i++) {
    cin >> kl[i];
    apps[kl[i][0] - 48] = apps[kl[i][1] - 48] = apps[kl[i][2] - 48] = 'x';
  }
  vector<char> r;
  for(int j = 0; j < 10; j++)
    if(apps[j] == 'x') r.push_back(j + 48);
  sort(r.begin(), r.end(), [&kl] (const char &a, const char &b) {
    for(int i = 0; i < 50; i++)
      if(b == kl[i][0] && (a == kl[i][1] || a == kl[i][2]) ||
         b == kl[i][1] && a == kl[i][2]) return false;
    return true;
  });
  for(vector<char>::iterator it = r.begin(); it != r.end(); it++)
    cout << *it;
  cout << endl;
}
