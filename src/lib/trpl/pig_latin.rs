pub fn to_pig_latin(s: String) -> String {
  let mut next_word = true;
  let mut goto_last = '\0';
  let mut r = String::new();
  for c in s.chars() {
    if next_word {
      if c == 'a' || c == 'e' || c == 'i' ||
          c == 'o' || c == 'u' {
        r.push(c);
        goto_last = 'h';
      } else {
        goto_last = c;
      }
      next_word = false;
    } else if c == ' ' {
      r = format!("{}-{}ay ", r, goto_last);
      next_word = true;
    } else {
      r.push(c);
    }
  }
  format!("{}-{}ay ", r, goto_last)
}
