#include <iostream>
#include <algorithm>

using namespace std;

struct Volunteer {
  int k; // 编号
  int s; // 分数
};

int main(int argc, char** argv) {
  int m, n; // m 选手总数；n 录取人数
  cin >> m >> n;
  int intervs = n * 1.5; // 分数线选手排名 == 面试人数
  Volunteer vols[m];
  for(int i = 0; i < m; i++)
    cin >> vols[i].k >> vols[i].s;
  sort(vols, vols + m, [](Volunteer v1, Volunteer v2) {
    if(v1.s == v2.s) return (v1.k < v2.k);
    else return (v1.s > v2.s);
  });
  while(vols[intervs - 1].s == vols[intervs].s) intervs++;
  cout << vols[intervs - 1].s << ' ' << intervs << endl;
  for(int i = 0; i < intervs; i++)
    cout << vols[i].k << ' ' << vols[i].s << endl;
  return 0;
}
