#include <iostream>
#include <string>

using namespace std;

int main() {
  int n;
  cin >> n;
  string mxname;
  int mxreward;
  int rew_sum = 0;
  for (int i = 0; i < n; i++) {
    string name;
    int avg_grade, cev_grade;
    char is_cadres, is_western;
    int paper;
    cin >> name >> avg_grade >> cev_grade
        >> is_cadres >> is_western >> paper;
    int reward = 0;
    if (avg_grade > 80 && paper > 0) reward += 8000;
    if (avg_grade > 85 && cev_grade > 80) reward += 4000;
    if (avg_grade > 90) reward += 2000;
    if (avg_grade > 85 && is_western == 'Y') reward += 1000;
    if (cev_grade > 80 && is_cadres == 'Y') reward += 850;
    if (reward > mxreward) {
      mxname = name;
      mxreward = reward;
    }
    rew_sum += reward;
  }
  cout << mxname << endl;
  cout << mxreward << endl;
  cout << rew_sum << endl;
  return 0;
}
