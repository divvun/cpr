use std::io::BufRead;

pub fn strip_all_escaped_newlines<R: BufRead>(reader: R) -> String {
    reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            if line.ends_with(r"\") {
                format!(" {}", line.trim_matches('\\').trim())
            } else {
                format!("{}\n", line.trim())
            }
        })
        .collect::<Vec<_>>()
        .join("")
}
