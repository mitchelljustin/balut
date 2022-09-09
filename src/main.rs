use ScanError::UnexpectedChar;

#[derive(Debug)]
enum ScanError {
    UnexpectedChar(char),
    MatchFailed { expected: char, actual: char },
}

#[derive(Debug)]
enum Token {
    Ident { name: String },
    Nomen { name: String },
    ColonColon,
}

struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    index: usize,
    start: usize,
}

impl Scanner {
    pub fn scan(source: String) -> Result<Vec<Token>, ScanError> {
        Self::new(source).scan_consume()
    }


    fn new(source: String) -> Scanner {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            index: 0,
            start: 0,
        }
    }

    fn current(&self) -> char {
        self.source[self.index]
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        match self.advance() {
            ':' => {
                self.consume(':')?;
                self.add(Token::ColonColon);
            }
            'a'..='z' | '_' => {
                self.ident_like(|name| Token::Ident { name });
            }
            'A'..='Z' => {
                self.ident_like(|name| Token::Nomen { name });
            }
            bad_char => return Err(UnexpectedChar(bad_char))
        }
        Ok(())
    }

    fn lexeme(&self) -> String {
        self.source[self.start..self.index].into_iter().collect()
    }

    fn scan_consume(mut self) -> Result<Vec<Token>, ScanError> {
        while !self.is_at_end() {
            self.start = self.index;
            self.scan_token()?;
        }
        return Ok(self.tokens);
    }

    fn consume(&mut self, expected: char) -> Result<(), ScanError> {
        let actual = self.advance();
        if actual != expected {
            return Err(ScanError::MatchFailed { actual, expected });
        }
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.current();
        self.increment();
        return c;
    }

    fn increment(&mut self) {
        self.index += 1;
    }

    fn add(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn ident_like(&mut self, make_token: impl FnOnce(String) -> Token) {
        self.consume_all_alphanum();
        let name = self.lexeme();
        self.add(make_token(name))
    }

    fn consume_all_alphanum(&mut self) {
        while !self.is_at_end() && matches!(self.current(), '0'..='9' | 'A'..='Z' | 'a'..='z' | '_') {
            self.increment();
        }
    }
}


fn main() {
    let source = "Some::Module::function".to_owned();
    let tokens = Scanner::scan(source).unwrap();
    println!("{tokens:?}")
}
