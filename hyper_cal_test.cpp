#include <./lib/hyper_cal/calc.h>
# include <cstring>
# include <cstdio>
# include <cmath>
#include <iostream>
#include <string>
#include <cstdlib>
#include <random>
#include <functional>

#define TEST_RADIX 10
#define NUM_OF_TESTS 25
#define TEST_RAND_MIN -1e12
#define TEST_RAND_MAX 1e12

using namespace std;

inline Num double_to_num(double n) {
  char str[40] = { '\0' };
  sprintf(str, "%.32g", n);
  Num res; res.In(str);
  return res;
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
  Num res_h = double_to_num(num);
  printf("initital value: %.32g\n", num);
  cout << endl;

  for (int i = 1; i <= NUM_OF_TESTS; i++) {
    char opc;
    num = rand_num();
    Num num_h = double_to_num(num);
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
    printf("%c %.32g:\n", opc, num);
    printf("  double result: %.309g\n", res_d);
    cout << "  hyper_cal result: "; res_h.Out(); cout << endl;
    cout << endl;
  }
  return 0;
}
