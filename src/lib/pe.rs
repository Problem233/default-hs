pub fn answer9_1() -> i64 {
  for a in 1..500 {
    for b in a..500 {
      let c = 1000 - a - b;
      if a * a + b * b == c * c {
        return a * b * c;
      }
    }
  }
  panic!("couldn't find any answer!")
}
