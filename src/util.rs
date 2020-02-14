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
