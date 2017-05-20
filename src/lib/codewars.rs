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
