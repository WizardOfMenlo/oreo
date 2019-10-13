#[derive(Debug, PartialEq, Clone)]
pub struct Scanned<'a> {
    pub(crate) inner: ScannedItem<'a>,
    pub(crate) line: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScannedItem<'a> {
    Str(&'a str),
    UnclosedStr(&'a str),
    Comment(&'a str),
    UnclosedComment(&'a str),
    Rest(&'a str),
}

pub fn scan(input: &str) -> impl Iterator<Item = Scanned> {
    LineScannerIt {
        input,
        pos: 0,
        line_no: 0,
    }
}

pub struct LineScannerIt<'a> {
    input: &'a str,
    pos: usize,
    line_no: usize,
}

fn count_new_lines(s: &str) -> usize {
    bytecount::count(s.as_bytes(), b'\n')
}

impl<'a> Iterator for LineScannerIt<'a> {
    type Item = Scanned<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let remaining = &self.input[self.pos..];

        // Skip the first whitespace chars
        let first_non_whitespace_index = remaining.find(|c: char| !c.is_whitespace());
        if remaining.is_empty() || first_non_whitespace_index.is_none() {
            return None;
        }

        let first_non_whitespace_index = first_non_whitespace_index.unwrap();
        self.line_no += count_new_lines(&remaining[..first_non_whitespace_index]);

        let remaining = &remaining[first_non_whitespace_index..];
        self.pos += first_non_whitespace_index;

        // Get the next chars
        let mut chars = remaining.chars().peekable();
        let first_char = chars.next().unwrap();

        let start_line_no = self.line_no;

        if first_char == '"' {
            let (result, read, lines) = parse_string(remaining);
            self.pos += read;
            self.line_no += lines;
            return Some(Scanned {
                inner: result,
                line: start_line_no,
            });
        } else if first_char == '{' {
            let second = chars.peek();
            if let Some('-') = second {
                let (result, read, lines) = parse_comment(remaining);
                self.pos += read;
                self.line_no += lines;
                return Some(Scanned {
                    inner: result,
                    line: start_line_no,
                });
            }
        }

        let (result, read, lines) = parse_block(remaining);
        self.pos += read;
        self.line_no += lines;
        Some(Scanned {
            inner: result,
            line: start_line_no,
        })
    }
}

fn parse_comment(input: &str) -> (ScannedItem, usize, usize) {
    let chars_pairs = input.chars().zip(input.chars().skip(1));
    let mut index = None;
    for (i, (first, second)) in chars_pairs.enumerate() {
        match (first, second) {
            ('-', '}') => {
                index = Some(i);
                break;
            }
            _ => continue,
        }
    }

    let lines = count_new_lines(index.map(|i| &input[2..i]).unwrap_or_else(|| &input[2..]));

    match index {
        Some(i) => (ScannedItem::Comment(&input[2..i]), i + 2, lines),
        None => (
            ScannedItem::UnclosedComment(&input[2..]),
            input.len(),
            lines,
        ),
    }
}

fn parse_string(input: &str) -> (ScannedItem, usize, usize) {
    let mut next_is_escape = false;
    let mut index = None;
    for (i, ch) in input.chars().enumerate().skip(1) {
        if !next_is_escape && ch == '\\' {
            next_is_escape = true;
        }

        if !next_is_escape && ch == '"' {
            index = Some(i);
        }

        next_is_escape = false;
    }

    let lines = count_new_lines(index.map(|i| &input[1..i]).unwrap_or_else(|| &input[1..]));

    match index {
        Some(i) => (ScannedItem::Str(&input[1..i]), i + 1, lines),
        None => (ScannedItem::UnclosedStr(&input[1..]), input.len(), lines),
    }
}

fn parse_block(input: &str) -> (ScannedItem, usize, usize) {
    let first_whitespace_index = input
        .find(|c: char| c.is_whitespace())
        .unwrap_or_else(|| input.len());

    let first_problematic = input
        .find(|c: char| c == '"' || c == '{')
        .unwrap_or_else(|| input.len());
    let to_read = std::cmp::min(first_whitespace_index, first_problematic);
    (ScannedItem::Rest(&input[0..to_read]), to_read, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn scan_empty() {
        let mut res = scan("");
        assert_eq!(res.next(), None);
    }

    #[test]
    fn scan_example() {
        let res: Vec<_> = scan("(x <= > >= < y 99.88l8 )").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_example_multiline() {
        let res: Vec<_> = scan("(x <= > >= < y 99.88l8 )\nx ) ( >= y").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_string_escaping() {
        let res: Vec<_> = scan("\"String \\\"inside\\\" a string\"").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_problematic() {
        let res: Vec<_> = scan("x\"something\"").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_comment() {
        let res: Vec<_> = scan("{-some comment-}").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_unclosed_comment() {
        let res: Vec<_> = scan("{-some comment").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_multiline_comment() {
        let res: Vec<_> = scan("{-some comment\n which is long! -}").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_real_life() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_real_life_with_comments() {
        let input = "program fib;\r\nbegin\r\nvar n;{-a comment here-}\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_oreo_8() {
        let input = r#"
        {- will parse OK -}
program test
                                   
begin                              
                                   
	var n;
	var first := 0;  
	var second :=1; 
	var next;                           
	var c :=0;
	
    print "enter the number of terms";
                 {- comment in body -}
    get n;  
    
    while ( first < (second > 32)  )
    begin

       if ( 42 < 100 ) then begin n := 5;  end
       			   else begin n := 46; end;
                        {- comment
                        over
                        multiple 
                        lines -}
                                  
    c := c + 1;	    
	end;
end    
        "#;
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }
}
