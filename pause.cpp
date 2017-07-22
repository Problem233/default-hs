#include <iostream>
#include <string>
#include <cstdio>
#include <conio.h>

using namespace std;

// text

void enter() {
  cout << "press enter to continue.." << endl;
}

void any_key() {
  cout << "press any key to continue.." << endl;
}

// use std only

void pause1() {
  enter();
  string empty;
  getline(cin, empty);
}

void pause2() {
  enter();
  getchar();
}

void pause3() {
  enter();
  cin.get();
}

// use windows api

void pause4() {
  any_key();
  getch();
}

// main

int main() {
  pause1();
  pause2();
  pause3();
  pause4();
  return 0;
}
