// analog to test.c in wren_test from wren_c

use std::fs;
use std::path::{Component, Path, PathBuf};

use crate::wren::*;

pub fn test_config() -> WrenConfiguration {
    WrenConfiguration {
        load_module_fn: Some(read_module),
        wren_write_fn: Some(write_string),
        resolve_module_fn: Some(resolve_module),
        error_fn: Some(error_fn),
        ..Default::default()
    }
}

fn error_fn(_vm: &WrenVM, error_type: WrenErrorType, module: &str, line: usize, message: &str) {
    match error_type {
        WrenErrorType::Compile => {
            // Matching test.c output:
            eprintln!("[{} line {}] {}", module, line, message);
        }
        // For runtime errors, wren_test expects:
        // Index must be a number.
        // [.test/core/string.../iterator_value_not_num line 1] in (script)
        WrenErrorType::Runtime => {
            eprintln!("{}", message);
        }
        WrenErrorType::StackTrace => {
            eprintln!("[{} line {}] in {}", module, line, message);
        }
    }
}

fn write_string(_vm: &WrenVM, string: &str) {
    print!("{}", string);
}

fn is_relative_import(module: &str) -> bool {
    module.starts_with(".")
}

fn normalize_path(path: &PathBuf) -> String {
    path.components()
        .map(|component| match component {
            Component::Prefix(s) => s.as_os_str().to_str().unwrap(),
            Component::RootDir => "",
            Component::CurDir => ".",
            Component::ParentDir => "..",
            Component::Normal(s) => s.to_str().unwrap(),
        })
        .collect::<Vec<&str>>()
        .join("/")
}

// Applies the CLI's import resolution policy. The rules are:
//
// * If [module] starts with "./" or "../", it is a relative import, relative
//   to [importer]. The resolved path is [name] concatenated onto the directory
//   containing [importer] and then normalized.
//
//   For example, importing "./a/./b/../c" from "./d/e/f" gives you "./d/e/a/c".
fn resolve_module(_vm: &WrenVM, importer: &str, module: &str) -> String {
    // Only relative imports need resolution?
    if !is_relative_import(module) {
        return module.into();
    }

    // Get the directory containing the importing module.
    let importer_path = Path::new(importer);
    let importer_dir = importer_path.parent().unwrap();
    // Add the relative import path.
    let resolved = importer_dir.join(module);
    normalize_path(&resolved)
}

fn read_module(_vm: &WrenVM, name: &str) -> Option<WrenLoadModuleResult> {
    // Hack for now.
    let path = format!("{}.wren", name);
    match fs::read_to_string(&path) {
        Ok(source) => return Some(WrenLoadModuleResult { source: source }),
        // Err(e) => {
        //     println!("{} : {}", path, e);
        // }
        _ => {}
    }
    None
}
