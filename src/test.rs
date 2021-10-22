// analog to test.c in wren_test from wren_c
// FIXME: Should not be part of safe_wren library.
// This is only here to share test_config between
// wren_debug.rs and wren_test.rs.

use std::fs;
use std::path::{Component, Path};

use crate::wren::*;

pub fn test_config() -> Configuration {
    Configuration {
        load_module_fn: Some(read_module),
        write_fn: Some(write_string),
        resolve_module_fn: Some(resolve_module),
        error_fn: Some(error_fn),
        ..Default::default()
    }
}

fn error_fn(_vm: &VM, error_type: ErrorType, module: &str, line: usize, message: &str) {
    match error_type {
        ErrorType::Compile => {
            // Matching test.c output:
            eprintln!("[{} line {}] {}", module, line, message);
        }
        // For runtime errors, wren_test expects:
        // Index must be a number.
        // [.test/core/string.../iterator_value_not_num line 1] in (script)
        ErrorType::Runtime => {
            eprintln!("{}", message);
        }
        ErrorType::StackTrace => {
            eprintln!("[{} line {}] in {}", module, line, message);
        }
    }
}

fn write_string(_vm: &VM, string: &str) {
    print!("{}", string);
}

fn is_relative_import(module: &str) -> bool {
    module.starts_with('.')
}

fn normalize_path(path: &Path) -> String {
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
fn resolve_module(_vm: &VM, importer: &str, module: &str) -> String {
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

fn read_module(_vm: &VM, name: &str) -> Option<LoadModuleResult> {
    // Hack for now.
    let path = format!("{}.wren", name);
    if let Ok(source) = fs::read_to_string(&path) {
        Some(LoadModuleResult { source })
    } else {
        None
    }
}
