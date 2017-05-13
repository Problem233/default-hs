#include <iostream>
#include <algorithm>

using namespace std;

int main() {
  int o[] = {7, 8, 9, 10};
  int i[] = {1, 2, 4};
  do {
    do {
      if(o[0] + 3 + i[0] == 14 && o[1] + i[0] + i[1] == 14 &&
         o[2] + i[1] + i[2] == 14 && o[3] + i[2] + 5 == 14)
        cout << 6 << 5 << 3 << o[0] << 3 << i[0]
             << o[1] << i[0] << i[1] << o[2] << i[1] << i[2]
             << o[3] << i[2] << 5 << endl;
    } while (next_permutation(i, i + 3));
  } while(next_permutation(o, o + 4));
  return 0;
}
