#include <./lib/hyper_cal/calc.h>
# include <cstring>
# include <cstdio>
# include <cmath>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <cstdlib>
#include <random>
#include <functional>

#define TEST_RADIX 10
#define NUM_OF_TESTS 25
#define DOUBLE_PREC setprecision(2048)
#define TEST_RAND_MIN -1e8
#define TEST_RAND_MAX 1e8

using namespace std;

inline char * double_to_cstring(double n) {
  ostringstream stout;
  stout << DOUBLE_PREC << n;
  const char * str = stout.str().c_str();
  char res[sizeof str] = { '\0' };
  return strcpy(res, str);
}

int main() {
  cout << "radix: " << TEST_RADIX << endl;
  cout << "number of tests: " << NUM_OF_TESTS << endl;
  cout << endl;

  default_random_engine rand_eng(__rdtsc());
  uniform_int_distribution<int> rand_op_dist(0, 3);
  auto rand_op = bind(rand_op_dist, rand_eng);
  uniform_real_distribution<double> rand_num_dist(TEST_RAND_MIN, TEST_RAND_MAX);
  auto rand_num = bind(rand_num_dist, rand_eng);

  double num = rand_num();
  double res_d = num;
  Num res_h; res_h.In(double_to_cstring(num));
  cout << "initital value: " << DOUBLE_PREC << num << endl;
  cout << endl;

  for (int i = 1; i <= NUM_OF_TESTS; i++) {
    char opc;
    num = rand_num();
    Num num_h; num_h.In(double_to_cstring(num));
    switch (rand_op()) {
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
    cout << opc << ' ' << DOUBLE_PREC << num << ":" << endl;
    cout << "  double result: " << DOUBLE_PREC << res_d << endl;
    cout << "  hyper_cal result: "; res_h.Out(); cout << endl;
    cout << endl;
  }
  return 0;
}
