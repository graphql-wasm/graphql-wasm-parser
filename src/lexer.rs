use std::ops::Range;
use std::slice::Iter;
use Result::*;
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    SOF,
    END_OF_INPUT,
    BANG,
    DOLLAR,
    AMP,
    PAREN_L,
    PAREN_R,
    SPREAD,
    COLON,
    EQUALS,
    AT,
    BRACKET_L,
    BRACKET_R,
    BRACE_L,
    PIPE,
    BRACE_R,
    NAME,
    INT,
    FLOAT,
    STRING,
    BLOCK_STRING,
    COMMENT,
}

#[derive(Debug)]
pub struct LexerError {
    pub message: String,
}
impl LexerError {
    pub fn new(message: &str) -> LexerError {
        return LexerError {
            message: String::from(message),
        };
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub value: Option<String>,
}

impl<'a> Token {
    pub fn simple_token(kind: TokenKind, index: usize) -> Token {
        Token {
            kind,
            start: index,
            end: index + 1,
            value: Option::None,
        }
    }

    pub fn get_value(self) -> String {
        self.value.as_ref().unwrap().clone()
    }
}

#[derive(Debug)]
pub struct Lexer {
    pub source: Vec<char>,
    pub current_index: usize,
    pub tokens: Vec<Token>,
}

impl<'a> Lexer {
    pub fn new(source_str: &'a str) -> Lexer {
        let start_of_file_token = Token {
            kind: TokenKind::SOF,
            start: 0,
            end: 0,
            value: Option::None,
        };
        Lexer {
            source: source_str.chars().collect(),
            current_index: 0,
            tokens: vec![start_of_file_token],
        }
    }
    pub fn current_token(&self) -> &Token {
        self.tokens.last().as_ref().expect("no tokens")
    }

    pub fn current_token_value(&self) -> String {
        let token = self.tokens.last().unwrap();
        token.value.as_ref().unwrap().clone()
    }

    pub fn current_token_value_safe(&self) -> Option<String> {
        let token = self.tokens.last().unwrap();
        match token.value.as_ref() {
            Some(s) => Some(s.clone()),
            None => None 
        }
    }

    pub fn prev_token(&self) -> &Token {
        self.tokens.get(self.tokens.len()-2).unwrap()
    }

    pub fn prev_token_value(&self) -> String {
        let token = self.tokens.get(self.tokens.len()-2).unwrap();
        token.value.as_ref().unwrap().clone()
    }

    pub fn advance_all(&mut self) {
        while self.tokens.last().expect("").kind != TokenKind::END_OF_INPUT {
            self.advance();
        }
    }

    pub fn lookahead(&self) -> Result<Token, LexerError> {
        if self.tokens.last().unwrap().kind == TokenKind::END_OF_INPUT {
            return Ok(Token::simple_token(TokenKind::END_OF_INPUT, self.source.len()))
        }
        let position = self.position_after_whitespace();
        let new_token;
        if position == self.source.len() {
            new_token = Token::simple_token(TokenKind::END_OF_INPUT, self.source.len());
        }else {
            new_token = self.read_token(position)?;
        }
        Ok(new_token)
    }
    pub fn advance(&mut self) -> Result<&Token, LexerError> {
        if self.tokens.last().unwrap().kind == TokenKind::END_OF_INPUT {
            return Ok(self.tokens.last().unwrap())
        }
        let position = self.position_after_whitespace();
        let new_token;
        if position == self.source.len() {
            new_token = Token::simple_token(TokenKind::END_OF_INPUT, self.source.len());
        }else {
            new_token = self.read_token(position)?;
        }
        self.current_index = new_token.end;
        self.tokens.push(new_token);
        Ok(&self.tokens.last().expect(""))
    }

    fn position_after_whitespace(&self) ->usize {
        let chars = &self.source[self.current_index..];
        let whitespaces_count = chars
            .iter()
            .take_while(Lexer::is_whitespace_or_ignored)
            .count();
        self.current_index + whitespaces_count
    }

    fn is_whitespace_or_ignored(c: &&char) -> bool {
        match c {
            '\u{0020}' | '\u{0009}' | '\u{feff}' | ',' => true,
            _ => false,
        }
    }

    fn read_token(&self, index: usize) -> Result<Token, LexerError> {
        let c = self.source[index];
        let new_token = match c {
            '{' => Ok(Token::simple_token(TokenKind::BRACE_L, index)),
            '}' => Ok(Token::simple_token(TokenKind::BRACE_R, index)),
            '!' => Ok(Token::simple_token(TokenKind::BANG, index)),
            '|' => Ok(Token::simple_token(TokenKind::PIPE, index)),
            ':' => Ok(Token::simple_token(TokenKind::COLON, index)),
            '[' => Ok(Token::simple_token(TokenKind::BRACKET_L, index)),
            ']' => Ok(Token::simple_token(TokenKind::BRACKET_R, index)),
            '$' => Ok(Token::simple_token(TokenKind::DOLLAR, index)),
            '@' => Ok(Token::simple_token(TokenKind::AT, index)),
            '&' => Ok(Token::simple_token(TokenKind::AMP, index)),
            '(' => Ok(Token::simple_token(TokenKind::PAREN_L, index)),
            ')' => Ok(Token::simple_token(TokenKind::PAREN_R, index)),
            '=' => Ok(Token::simple_token(TokenKind::EQUALS, index)),
            '\"' => self.read_string_or_block_string(index),
            '.' => self.read_spread(index),
            'A'..='Z' | '_' | 'a'..='z' => Ok(self.read_name(index)),
            '0'..='9' | '-' => self.read_number(index),
            '#' => Ok(self.read_comment(index)),
            _ => panic!("unexpected char '{}'", c.escape_unicode()),
        }?;

        Ok(new_token)
    }

    fn read_spread(&self,index: usize) -> Result<Token, LexerError> {
        // we have at least a .
        let mut iter = (&self.source[index+ 1..]).iter();
        let next_char_1 = iter.next();
        Lexer::expect_char(next_char_1, '.')?;
        let next_char_2 = iter.next();
        Lexer::expect_char(next_char_2, '.')?;
        Ok(Token {
            kind: TokenKind::SPREAD,
            start: index,
            end: index + 3,
            value: None,
        })
    }
    fn expect_char(c: Option<&char>, expected_char: char) -> Result<(), LexerError> {
        match c {
            None => Err(LexerError::new("Unexpected char")),
            Some(actual_char) => {
                if *actual_char != expected_char {
                    Err(LexerError::new("Unexpected char"))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn read_comment(&self,index: usize) -> Token {
        let mut value = String::new();
        // we have at least `#`
        let iter = (&self.source[index + 1..]).iter();
        let mut counter = 1;
        for c in iter {
            match *c {
                '\u{000A}' | '\u{000D}' => {
                    counter += 1;
                    break;
                }
                _ => {
                    counter += 1;
                    value.push(*c);
                }
            }
        }
        Token {
            kind: TokenKind::COMMENT,
            start: index,
            end: index + counter,
            value: Option::Some(value),
        }
    }

    fn read_name(&self,index: usize) -> Token {
        let string = &self.source[index..];
        let mut name_string = String::new();
        let mut counter = 0;
        for c in string.iter() {
            match *c {
                '_' | '0'..='9' | 'A'..='Z' | 'a'..='z' => {
                    counter += 1;
                    name_string.push(*c);
                }
                _ => break,
            }
        }
        Token {
            kind: TokenKind::NAME,
            start: index,
            end: index + counter,
            value: Option::Some(name_string),
        }
    }

    fn read_number(&self,index: usize) -> Result<Token, LexerError> {
        let first_code = self.source[index];
        let mut code = Option::Some(first_code);
        let mut position = index;
        let mut is_float = false;
        if code.is_some() && code.unwrap() == '-' {
            position += 1;
            code = self.source.get(position).map(|c| *c);
        };
        if code.is_some() && code.unwrap() == '0' {
            position += 1;
            code = self.source.get(position).map(|c| *c);
            if code.is_some() {
                if code.unwrap() >= '0' && code.unwrap() <= '9' {
                    return Err(LexerError::new(&format!("invalid char {}", code.unwrap())));
                }
            }
        } else {
            position = self.assert_and_read_digits(position, code)?;
            code = self.source.get(position).map(|c| *c);
        }
        if code.is_some() && code.unwrap() == '.' {
            is_float = true;
            position += 1;
            code = self.source.get(position).map(|c| *c);
            position = self.assert_and_read_digits(position, code)?;
            code = self.source.get(position).map(|c| *c);
        }
        if code.is_some() && (code.unwrap() == 'E' || code.unwrap() == 'e') {
            is_float = true;
            position += 1;
            code = self.source.get(position).map(|c| *c);
            if code.is_some() && code.unwrap() == '+' || code.unwrap() == '-' {
                position += 1;
                code = self.source.get(position).map(|c| *c);
            }
            position = self.assert_and_read_digits(position, code)?;
        }
        let value: String = self.source[index..position]
            .iter()
            .cloned()
            .collect::<String>();
        Ok(Token {
            kind: if is_float {
                TokenKind::FLOAT
            } else {
                TokenKind::INT
            },
            start: index,
            end: position,
            value: Option::Some(value),
        })
    }


    fn assert_and_read_digits(
        &self,
        start: usize,
        first_code: Option<char>,
    ) -> Result<usize, LexerError> {
        if first_code.is_none() {
            return Err(LexerError::new(&format!(
                "Invalid number, expected digit but got end of input"
            )));
        }
        if !(first_code.unwrap() >= '0' && first_code.unwrap() <= '9') {
            return Err(LexerError::new(&format!(
                "Invalid number, expected digit but got {}",
                first_code.unwrap()
            )));
        }
        let string = &self.source[start..];
        let iter = string.iter();
        Ok(iter.take_while(|&&c| c >= '0' && c <= '9').count() + start)
    }

    fn read_string_or_block_string(&self, index:usize) -> Result<Token, LexerError> {
        if self.is_triple_quote(index) {
            return self.read_block_string(index);
        } else {
            return self.read_string(index);
        }
    }

    fn read_string(&self,index: usize) -> Result<Token, LexerError> {
        // we have at least "
        let string = &self.source[index + 1..];
        let mut value = String::new();

        let mut counter = 1;
        for c in string.iter() {
            counter += 1;
            if *c == '"' {
                return Ok(Token {
                    kind: TokenKind::STRING,
                    start: index,
                    end: index + counter,
                    value: Option::Some(value),
                });
            }
            if *c < '\u{0020}' && *c != '\u{0009}' {
                return Err(LexerError::new("not allowed char in String"));
            }
            value.push(*c);
        }
        Err(LexerError::new("Unterminated string."))
    }

    fn read_block_string(&self,index:usize) -> Result<Token, LexerError> {
        // we know it starts with triple quote
        let mut position = index + 3;
        let mut chunk_start = position;
        let mut raw_value = String::new();
        while position < self.source.len() {
            if self.is_triple_quote(position) {
                raw_value.push_str(
                    &self.source[chunk_start..position]
                        .iter()
                        .cloned()
                        .collect::<String>(),
                );
                return Ok(Token {
                    kind: TokenKind::BLOCK_STRING,
                    start: index,
                    end: position+3,
                    value: Option::Some(raw_value),
                });
            }
            let code = self.source[position];
            if code < '\u{0020}' && code != '\u{0009}' && code != '\u{000a}' && code != '\u{000d}' {
                return Err(LexerError::new(&format!(
                    "Invalid character within String: {}.",
                    code
                )));
            }
            if code == '\u{000a}' {
                position += 1;
            } else if code == '\u{000d}' {
                if self.char_equal_at(position + 1, '\u{000a}') {
                    // CRLF means one newline
                    position += 2;
                } else {
                    position += 1;
                }
            } else if code == '\u{005c}' && self.is_triple_quote(position + 1) {
                raw_value.push_str(
                    &self.source[chunk_start..position]
                        .iter()
                        .cloned()
                        .collect::<String>(),
                );
                raw_value.push_str("\"\"\"");
                position += 4;
                chunk_start = position;
            } else {
                position += 1;
            }
        }
        Err(LexerError::new("Untermintated block string"))
    }

    fn is_triple_quote(&self, position: usize) -> bool {
        if !(position + 2 < self.source.len()) {
            return false;
        }
        self.source[position] == '"'
            && self.source[position + 1] == '"'
            && self.source[position + 2] == '"'
    }

    fn char_equal_at(&self, position: usize, c: char) -> bool {
        match self.source.get(position) {
            Some(value) => *value == c,
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! test_token {
        ($name:ident $source:literal $token_kind:ident) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($source);
                lexer.advance_all();
                assert_eq!(lexer.tokens[1].kind, $token_kind);
            }
        };
        ($name:ident $source:literal $token_kind:ident $value:literal) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($source);
                lexer.advance_all();
                assert_eq!(lexer.tokens[1].kind, $token_kind);
                assert_eq!(lexer.tokens[1].value, Some(String::from($value)));
            }
        };
    }

    use super::TokenKind::*;
    use super::*;

    test_token!(name_starting_with_underscoe "_someName" NAME "_someName");
    test_token!(block_string r#""""string""""# BLOCK_STRING "string");
    test_token!(block_string_triple_quote_escaped r#""""string\"""""""# BLOCK_STRING r#"string""""#);
    test_token!(bang "!" BANG);
    test_token!(equals "=" EQUALS);
    test_token!(float_e_notation "123e4" FLOAT "123e4");
    test_token!(float_e_notation_2 "123E4" FLOAT "123E4");
    test_token!(float_e_notation_3 "57.123E4" FLOAT "57.123E4");
    test_token!(float_e_notation_4 "26.123e4" FLOAT "26.123e4");

    #[test]
    fn parse_simple_query() {
        let mut lexer = Lexer::new(" {foo}");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 5);
    }
    #[test]
    fn parse_name_starting_with_underscore() {
        let mut lexer = Lexer::new("_someName");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, NAME);
        assert_eq!(lexer.tokens[1].value, Some(String::from("_someName")));
    }
    #[test]
    fn parse_bang() {
        let mut lexer = Lexer::new("!");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, BANG);
    }
    #[test]
    fn parse_pipe() {
        let mut lexer = Lexer::new("|");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, PIPE);
    }
    #[test]
    fn parse_colon() {
        let mut lexer = Lexer::new(":");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, COLON);
    }
    #[test]
    fn parse_bracket() {
        let mut lexer = Lexer::new("[]");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, BRACKET_L);
        assert_eq!(lexer.tokens[2].kind, BRACKET_R);
    }
    #[test]
    fn parse_parentheses() {
        let mut lexer = Lexer::new("()");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, PAREN_L);
        assert_eq!(lexer.tokens[2].kind, PAREN_R);
    }
    #[test]
    fn parse_dollar() {
        let mut lexer = Lexer::new("$");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, DOLLAR);
    }
    #[test]
    fn parse_at() {
        let mut lexer = Lexer::new("@");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, AT);
    }
    #[test]
    fn parse_ampersand() {
        let mut lexer = Lexer::new("&");
        lexer.advance_all();
        assert_eq!(lexer.tokens[1].kind, AMP);
    }
    #[test]
    fn parse_simple_query_with_ws() {
        let mut lexer = Lexer::new(", { foo } ");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 5);
    }

    #[test]
    fn parse_simple_query_with_ws_2() {
        let mut lexer = Lexer::new(" { foo } ");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 5);
    }
    #[test]
    fn parse_simple_query_with_comment() {
        let mut lexer = Lexer::new(" #hello\n { foo } ");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 6);
    }
    #[test]
    fn parse_string() {
        let mut lexer = Lexer::new(" \" foo \" ");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let token = &lexer.tokens[1];
        assert_eq!(token.kind, TokenKind::STRING);
        let value = token.value.as_ref().expect("");
        assert_eq!(value, " foo ");
    }

    #[test]
    fn parse_comment() {
        let mut lexer = Lexer::new("#comment");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let comment_token = &lexer.tokens[1];
        assert_eq!(comment_token.kind, TokenKind::COMMENT);
        let value = comment_token.value.as_ref().expect("");
        assert_eq!(value, "comment");
    }
    #[test]
    fn parse_integer() {
        let mut lexer = Lexer::new("1234");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let integer_token = &lexer.tokens[1];
        assert_eq!(integer_token.kind, TokenKind::INT);
        let value = integer_token.value.as_ref().expect("");
        assert_eq!(value, "1234");
    }

    #[test]
    fn parse_float() {
        let mut lexer = Lexer::new("0.123");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let integer_token = &lexer.tokens[1];
        assert_eq!(integer_token.kind, TokenKind::FLOAT);
        let value = integer_token.value.as_ref().expect("");
        assert_eq!(value, "0.123");
    }
    #[test]
    fn parse_minus_zero() {
        let mut lexer = Lexer::new("-0");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let integer_token = &lexer.tokens[1];
        assert_eq!(integer_token.kind, TokenKind::INT);
        let value = integer_token.value.as_ref().expect("");
        assert_eq!(value, "-0");
    }
    #[test]
    fn parse_zero() {
        let mut lexer = Lexer::new("0");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let integer_token = &lexer.tokens[1];
        assert_eq!(integer_token.kind, TokenKind::INT);
        let value = integer_token.value.as_ref().expect("");
        assert_eq!(value, "0");
    }
    #[test]
    fn parse_minus_int() {
        let mut lexer = Lexer::new("-4");
        lexer.advance_all();
        assert_eq!(lexer.tokens.len(), 3);
        let integer_token = &lexer.tokens[1];
        assert_eq!(integer_token.kind, TokenKind::INT);
        let value = integer_token.value.as_ref().expect("");
        assert_eq!(value, "-4");
    }
}
