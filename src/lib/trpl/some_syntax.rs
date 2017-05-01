pub fn if_syntax(cond: bool) {
  if cond {
    println!("true!");
  } else {
    println!("false!");
  }
}

pub fn match_syntax<T>(option: Option<T>) {
  match option {
    None => println!("None!"),
    Some(_) => println!("Some!")
  };
}

pub fn if_let_syntax<T>(option: Option<T>) {
  if let Some(_) = option {
    println!("There is something!")
  }
}
