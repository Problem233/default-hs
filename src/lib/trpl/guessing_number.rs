extern crate rand;

use std::io;
use std::io::Write;
use std::cmp::Ordering;
use rand::Rng;

pub fn play() {
  println!("Guess the number!");
  let secret = rand::thread_rng().gen_range(1, 101);
  loop {
    print!("Please input your guess: ");
    io::stdout().flush()
      .expect("Failed to flush stdout!");
    let mut guess = String::new();
    io::stdin().read_line(&mut guess)
      .expect("Failed to read line!");
    let guess: u32 = match guess.trim().parse() {
      Ok(x) => x,
      Err(_) => {
        println!("Please input a number!");
        continue;
      }
    };
    match guess.cmp(&secret) {
      Ordering::Less    => println!("Too small!"),
      Ordering::Greater => println!("Too big!"),
      Ordering::Equal   => {
        println!("You win!");
        break;
      }
    };
  }
}
