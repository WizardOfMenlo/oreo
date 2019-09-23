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
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = scan(input).collect();
        assert_debug_snapshot!(res);
    }
}
