use super::error::error_bind_method;
use wren_rust::wren::{WrenForeignMethodFn, WrenVM};

pub fn api_test_bind_foreign_method_fn(
    _vm: &WrenVM,
    _module: &str,
    class_name: &str,
    is_static: bool,
    method_name: &str,
) -> Option<WrenForeignMethodFn> {
    // For convenience, concatenate all of the method qualifiers into a single
    // signature string.
    let full_signature = if is_static {
        format!("static {}.{}", class_name, method_name)
    } else {
        format!("{}.{}", class_name, method_name)
    };

    if let Some(bind_fn) = error_bind_method(&full_signature) {
        return Some(bind_fn);
    }
    None
}
