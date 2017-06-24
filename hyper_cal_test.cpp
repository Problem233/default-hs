#include <./lib/hyper_cal/calc.h>
# include <cstring>
# include <cstdio>
# include <cmath>
#include <iostream>
#include <iomanip>
#include <string>
#include <cstdlib>
#include <ctime>

#define TEST_RADIX 10
#define NUM_OF_TESTS 25
#define RAND_MAX 0x7fff

using namespace std;

int main() {
  cout << "radix: " << TEST_RADIX << endl;
  cout << "number of tests: " << NUM_OF_TESTS << endl;
  cout << endl;

  srand(time(0));
  int num = rand();
  double res_d = num;
  Num res_h; res_h.In(num);
  cout << "initital value: " << num << endl;
  cout << endl;

  for (int i = 1; i <= NUM_OF_TESTS; i++) {
    int op = rand() % 4;
    char opc;
    num = rand();
    Num num_h; num_h.In(num);
    switch (rand() % 4) {
      case 0: opc = '+';
              res_d = res_d + num;
              res_h = res_h + num_h;
              break;
      case 1: opc = '-';
              res_d = res_d - num;
              res_h = res_h - num_h;
              break;
      case 2: opc = '*';
              res_d = res_d * num;
              res_h = res_h * num_h;
              break;
      case 3: opc = '/';
              res_d = res_d / num;
              res_h = res_h / num_h;
              break;
    }
    cout << opc << ' ' << num << ":" << endl;
    cout << "  double result: " << setprecision(32) << res_d << endl;
    cout << "  hyper_cal result: "; res_h.Out(); cout << endl;
    cout << endl;
  }
  return 0;
}
