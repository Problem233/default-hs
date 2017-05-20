pub fn diophantine_equation(n: i64) -> Vec<(i32, i32)> {
  let mut r: Vec<(i32, i32)> = Vec::new();
  for i in 1..((n as f64).sqrt() as i64) {
    if n % i == 0 {
      let j = n / i;
      if (j - i) % 4 == 0 {
        let y = (j - i) / 4;
        let x = i + 2 * y;
        r.push((x as i32, y as i32));
      }
    }
  }
  r
}

pub fn is_my_friend_cheating(n: i64) -> Vec<(i64, i64)> {
  fn ceildiv(a: i64, b: i64) -> i64 {
    (a - 1) / b + 1
  }
  let mut r: Vec<(i64, i64)> = Vec::new();
  let sum = (1 + n) * n / 2;
  let min = ceildiv(n * (n - 1), (2 * (n + 1)));
  for x in min..n + 1 {
    if (sum - x) % (x + 1) == 0 {
      r.push((x, (sum - x) / (x + 1)));
    }
  }
  r
}
