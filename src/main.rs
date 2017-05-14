extern crate rand;
extern crate permutohedron;

pub mod lib;

use lib::trpl::pig_latin::*;

fn main() {
  println!("{}", to_pig_latin(String::from("to harder math problems")));
}
