use std::env;
use std::fs;
use std::path::Path;

fn comment_out(before: String, line: &str) -> String {
    let after = before.replace(line, &format!("// {}", line));
    assert_ne!(before, after); // Panic if our replace didn't work!
    after
}

fn generate_wren_core_source_rs() {
    let wren_core_wren_path = Path::new("wren_c/src/vm/wren_core.wren");
    let source = fs::read_to_string(wren_core_wren_path).unwrap();
    // Fiber and Fn are both defined in code in safe_wren.
    let modified = comment_out(comment_out(source, "class Fiber {}"), "class Fn {}");
    let code = format!(
        "pub fn wren_core_source() -> &'static str {{
        r#\"{}\"#
    }}
    ",
        modified
    );

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("wren_core_source.rs");
    fs::write(&dest_path, code).unwrap();

    println!("cargo:rerun-if-changed={}", wren_core_wren_path.display());
    println!("cargo:rerun-if-changed=build.rs");
}

fn compile_opt_random() {
    // FIXME: The analyzer seems even slower now.
    // Is this rebuilding too often?
    let opt_random_path = "wren_c/src/optional/wren_opt_random.c";
    cc::Build::new()
        .file(opt_random_path)
        .include("wren_c/src/include") // For wren.h
        .include("wren_c/src/vm") // For wren_common.h
        .warnings(false) // Big hammer to disable unused-parameter warnings. :/
        .compile("wren_opt_random");
    println!("cargo:rerun-if-changed={}", opt_random_path);
}

fn main() {
    generate_wren_core_source_rs();
    // compile_opt_random();
}
