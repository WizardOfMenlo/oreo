#[derive(Debug, PartialEq)]
pub struct Scanned<'a> {
    pub(crate) inner: ScannedItem<'a>,
    pub(crate) line: usize,
}

#[derive(Debug, PartialEq)]
pub enum ScannedItem<'a> {
    Str(&'a str),
    UnclosedStr(&'a str),
    Rest(&'a str),
}

pub fn scan(input: &str) -> impl Iterator<Item = Scanned> {
    input
        .lines()
        .enumerate()
        .map(|(i, line)| {
            scan_line(line).map(move |scan| Scanned {
                inner: scan,
                line: i,
            })
        })
        .flatten()
}

fn scan_line(line: &str) -> impl Iterator<Item = ScannedItem> {
    LineScannerIt { line, pos: 0 }
}

pub struct LineScannerIt<'a> {
    line: &'a str,
    pos: usize,
}

impl<'a> Iterator for LineScannerIt<'a> {
    type Item = ScannedItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let remaining = &self.line[self.pos..];
        let first_non_whitespace_index = remaining.find(|c: char| !c.is_whitespace());
        if remaining.is_empty() || first_non_whitespace_index.is_none() {
            return None;
        }

        let remaining = &remaining[first_non_whitespace_index.unwrap()..];
        self.pos += first_non_whitespace_index.unwrap();

        let mut chars = remaining.chars();
        let first_char = chars.next().unwrap();

        if first_char == '"' {
            let (result, read) = parse_string(remaining);
            self.pos += read;
            Some(result)
        } else {
            let (result, read) = parse_block(remaining);
            self.pos += read;
            result
        }
    }
}

fn parse_string(input: &str) -> (ScannedItem, usize) {
    let mut next_is_escape = false;
    let mut index = None;
    for (i, ch) in input.chars().enumerate().skip(1) {
        if !next_is_escape && ch == '\\' {
            next_is_escape = true;
        }

        if next_is_escape {
            next_is_escape = false;
        }

        if !next_is_escape && ch == '"' {
            index = Some(i);
        }
    }

    match index {
        Some(i) => (ScannedItem::Str(&input[1..i]), i + 1),
        None => (ScannedItem::UnclosedStr(&input[1..]), input.len()),
    }
}

fn parse_block(input: &str) -> (Option<ScannedItem>, usize) {
    let first_whitespace_index = input
        .find(|c: char| c.is_whitespace())
        .unwrap_or_else(|| input.len());
    (
        Some(ScannedItem::Rest(&input[0..first_whitespace_index])),
        first_whitespace_index,
    )
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
    fn scan_problematic() {
        let res: Vec<_> = scan("var x\"something\"").collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn scan_real_life() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }
}
