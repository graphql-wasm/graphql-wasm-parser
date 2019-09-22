#[macro_use]
extern crate serde_derive;

use wasm_bindgen::prelude::*;
pub mod lexer;
pub mod parser;
pub mod ast;

#[wasm_bindgen]
pub fn parse(document: &str) -> JsValue {
    let mut lexer = lexer::Lexer::new(document);
    let result = parser::parse(&mut lexer).unwrap();
    JsValue::from_serde(&result).unwrap()
}
