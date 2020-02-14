use std::sync::Mutex;

lazy_static! {
    static ref IDENT_COUNT: Mutex<i32> = Mutex::new(0);
}

pub fn fresh_ident() -> String {
    let ref mut count = *IDENT_COUNT.lock().unwrap();
    let res = format!("<dummy{}>", count);
    *count = *count + 1;
    res
}

pub fn input_yes_or_no(msg: &str) -> bool {
    eprint!("{}", msg);
    loop {
        use std::io::stdin;
        let stdin = stdin();
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();
        match &line.trim() {
            &"y" => return true,
            &"n" => return false,
            _ => (),
        }
        eprint!("answer 'y' or 'no': ");
    }
}
