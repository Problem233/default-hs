#include "./lib/hyper_cal/calc.h"
#include <cstring>
#include <cstdio>
#include <cmath>
#include <iostream>
#include <string>
#include <cstdlib>
#include <random>
#include <functional>

using namespace std;

inline Num double_to_num(long double n) {
  char str[70] = { '\0' };
  sprintf(str, "%.64Lg", n);
  Num res; res.In(str);
  return res;
}

int main(int argc, char * argv[]) {
  if (argc != 4) {
    cout << "illegal arguments" << endl;
    cout << "usage: hyper_cal_test"
         << " <num_of_tests> <rand_min> <rand_max>" << endl;
    return 1;
  }
  int num_tests = atoi(argv[1]);
  long double rand_min = atof(argv[2]);
  long double rand_max = atof(argv[3]);

  default_random_engine rand_eng(__rdtsc());
  uniform_int_distribution<int> rand_op_dist(0, 3);
  auto rand_op = bind(rand_op_dist, rand_eng);
  uniform_real_distribution<long double> rand_num_dist(rand_min, rand_max);
  auto rand_num = bind(rand_num_dist, rand_eng);

  long double num = (long double) rand_num();
  long double res_d = num;
  Num res_h = double_to_num(num);
  printf("initital value: %.64Lg\n", num);
  cout << endl;

  for (int i = 1; i <= num_tests; i++) {
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
    cout << '#' << i << ':' << endl;
    printf("  operator: %c  num: %.64Lg\n", opc, num);
    printf("     double result: %.64Lg\n", res_d);
    cout << "  hyper_cal result: "; res_h.Out(); cout << endl;
    cout << endl;
  }
  return 0;
}
