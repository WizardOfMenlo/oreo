#[derive(Debug, PartialEq)]
pub struct Scanned<'a> {
    pub(crate) inner: &'a str,
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
            line.split_whitespace().map(move |scan| Scanned {
                inner: scan,
                line: i,
            })
        })
        .flatten()
}

struct ScanLineIterator<'a> {
    input: &'a str,
    current_position: usize,
}

impl<'a> Iterator for ScanLineIterator<'a> {
    type Item = ScannedItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let remain = &self.input[self.current_position..];
        if remain.len() == 0 {
            return None;
        }

        let mut chars = remain.chars();
        let sentinel = chars.next().unwrap();

        if sentinel != '"' {
            let next_index = remain.find('"').unwrap_or_else(|| self.input.len());
            return Some(ScannedItem::Rest(&remain[0..next_index]));
        } else {
            // Find the index of the next " (No escapes atm);
            let next_index = &remain[1..].find('"');
            match next_index {
                // TODO: Here we might want to keep the "
                Some(num) => Some(ScannedItem::Str(&remain[1..*num])),
                None => Some(ScannedItem::UnclosedStr(&remain[1..])),
            }
        }
    }
}

fn scan_line(input: &str) -> impl Iterator<Item = ScannedItem> {
    ScanLineIterator {
        input,
        current_position: 0,
    }
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
    fn test_real_life() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }
}
