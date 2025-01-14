use std::process::{Command, Output};

use lang_c::driver::Config;
// thanks chatgpt (i wrote the rest though)
pub fn preprocess_file(file_path: &str, clang_path: &str) -> std::io::Result<String> {
    // Run Clang with the -E flag to preprocess the file
    let output: Output = Command::new(clang_path)
        .arg("-E")
        .arg(file_path)
        .output()?;
    
    // Check if Clang succeeded
    if !output.status.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!(
                "Clang failed with status {}: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            ),
        ));
    }

    // Convert the output to a String
    let preprocessed_output = String::from_utf8(output.stdout)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))?;

    Ok(preprocessed_output)
}

fn main() {
    let input = include_str!("./c/test.c");
    let parse = lang_c::driver::parse(&Config::with_clang(), "./src/c/test.c").unwrap();
    dbg!(&parse.unit);
    glsc_hir_lower::HirLower{}.lower_translation_unit(&parse.unit);
}
