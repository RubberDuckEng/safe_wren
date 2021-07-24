// analog to wren.h from wren_c.

pub use crate::vm::WrenVM;

pub static WREN_VERSION_STRING: &str = "wren_rust-0.1";

// A function callable from Wren code, but implemented in another language.
// FIXME: How does this report errors?
pub type WrenForeignMethodFn = fn(vm: &WrenVM);

// Gives the host a chance to canonicalize the imported module name,
// potentially taking into account the (previously resolved) name of the module
// that contains the import. Typically, this is used to implement relative
// imports.
type WrenResolveModuleFn = fn(vm: &WrenVM, importer: &str, name: &str) -> String;

// The result of a loadModuleFn call.
// [source] is the source code for the module.
pub struct WrenLoadModuleResult {
    pub source: String,
}

// Loads and returns the source code for the module [name] if found.
type WrenLoadModuleFn = fn(vm: &WrenVM, name: &str) -> Option<WrenLoadModuleResult>;

// Returns a pointer to a foreign method on [className] in [module] with
// [signature].
type WrenBindForeignMethodFn = fn(
    vm: &WrenVM,
    module: &str,
    class_name: &str,
    is_static: bool,
    signature: &str,
) -> Option<WrenForeignMethodFn>;

// Displays a string of text to the user.
type WrenWriteFn = fn(vm: &WrenVM, text: &str);

pub enum WrenErrorType {
    // A syntax or resolution error detected at compile time.
    Compile,

    // The error message for a runtime error.
    Runtime,

    // One entry of a runtime error's stack trace.
    StackTrace,
}

// Reports an error to the user.
//
// An error detected during compile time is reported by calling this once with
// [type] `WREN_ERROR_COMPILE`, the resolved name of the [module] and [line]
// where the error occurs, and the compiler's error [message].
//
// A runtime error is reported by calling this once with [type]
// `WREN_ERROR_RUNTIME`, no [module] or [line], and the runtime error's
// [message]. After that, a series of [type] `WREN_ERROR_STACK_TRACE` calls are
// made for each line in the stack trace. Each of those has the resolved
// [module] and [line] where the method or function is defined and [message] is
// the name of the method or function.
type WrenErrorFn =
    fn(vm: &WrenVM, error_type: WrenErrorType, module: &str, line: usize, message: &str);

pub struct WrenForeignClassMethods {
    // The callback invoked when the foreign object is created.
    //
    // This must be provided. Inside the body of this, it must call
    // [wrenSetSlotNewForeign()] exactly once.
    pub allocate: WrenForeignMethodFn,
    // The callback invoked when the garbage collector is about to collect a
    // foreign object's memory.
    //
    // This may be `None` if the foreign class does not need to finalize.
    // pub finalize: Option<WrenFinalizerFn>,
}

// Returns a pair of pointers to the foreign methods used to allocate and
// finalize the data for instances of [className] in resolved [module].
pub type WrenBindForeignClassFn =
    fn(vm: &WrenVM, module_name: &str, class_name: &str) -> WrenForeignClassMethods;

// FIXME: derive Default is a hack for now.
#[derive(Default)]
pub struct WrenConfiguration {
    // The callback Wren uses to resolve a module name.
    //
    // Some host applications may wish to support "relative" imports, where the
    // meaning of an import string depends on the module that contains it. To
    // support that without baking any policy into Wren itself, the VM gives the
    // host a chance to resolve an import string.
    //
    // Before an import is loaded, it calls this, passing in the name of the
    // module that contains the import and the import string. The host app can
    // look at both of those and produce a new "canonical" string that uniquely
    // identifies the module. This string is then used as the name of the module
    // going forward. It is what is passed to [loadModuleFn], how duplicate
    // imports of the same module are detected, and how the module is reported in
    // stack traces.
    //
    // If you leave this function NULL, then the original import string is
    // treated as the resolved string.
    //
    // If an import cannot be resolved by the embedder, it should return NULL and
    // Wren will report that as a runtime error.
    //
    // Wren will take ownership of the string you return and free it for you, so
    // it should be allocated using the same allocation function you provide
    // above.
    pub resolve_module_fn: Option<WrenResolveModuleFn>,

    // The callback Wren uses to load a module.
    //
    // Since Wren does not talk directly to the file system, it relies on the
    // embedder to physically locate and read the source code for a module. The
    // first time an import appears, Wren will call this and pass in the name of
    // the module being imported. The method will return a result, which contains
    // the source code for that module. Memory for the source is owned by the
    // host application, and can be freed using the onComplete callback.
    //
    // This will only be called once for any given module name. Wren caches the
    // result internally so subsequent imports of the same module will use the
    // previous source and not call this.
    //
    // If a module with the given name could not be found by the embedder, it
    // should return NULL and Wren will report that as a runtime error.
    pub load_module_fn: Option<WrenLoadModuleFn>,

    // The callback Wren uses to find a foreign method and bind it to a class.
    //
    // When a foreign method is declared in a class, this will be called with the
    // foreign method's module, class, and signature when the class body is
    // executed. It should return a pointer to the foreign function that will be
    // bound to that method.
    //
    // If the foreign function could not be found, this should return NULL and
    // Wren will report it as runtime error.
    pub bind_foreign_method_fn: Option<WrenBindForeignMethodFn>,

    // The callback Wren uses to find a foreign class and get its foreign methods.
    //
    // When a foreign class is declared, this will be called with the class's
    // module and name when the class body is executed. It should return the
    // foreign functions uses to allocate and (optionally) finalize the bytes
    // stored in the foreign object when an instance is created.
    pub bind_foreign_class_fn: Option<WrenBindForeignClassFn>,

    // The callback Wren uses to display text when `System.print()` or the other
    // related functions are called.
    //
    // If this is `NULL`, Wren discards any printed text.
    pub wren_write_fn: Option<WrenWriteFn>,

    // The callback Wren uses to report errors.
    //
    // When an error occurs, this will be called with the module name, line
    // number, and an error message. If this is None, Wren doesn't report any
    // errors.
    pub error_fn: Option<WrenErrorFn>,

    // FIXME: Hack during development, shouldn't be API.
    pub debug_level: Option<DebugLevel>,
}

#[allow(dead_code)]
pub enum DebugLevel {
    NonCore,
    All,
}

pub enum WrenInterpretResult {
    Success,
    CompileError,
    RuntimeError,
}

// Runs [source], a string of Wren source code in a new fiber in [vm] in the
// context of resolved [module].
pub fn wren_interpret(vm: &mut WrenVM, module: &str, source: String) -> WrenInterpretResult {
    match crate::compiler::wren_compile_source(vm, module, source) {
        Err(e) => {
            if let Some(error_fn) = vm.config.error_fn {
                error_fn(
                    vm,
                    WrenErrorType::Compile,
                    module,
                    e.line,
                    &e.error.to_string(),
                );
            }
            WrenInterpretResult::CompileError
        }
        Ok(closure) => {
            // wren_c does the fiber creation here instead.
            match vm.run(closure) {
                Err(e) => {
                    if let Some(error_fn) = vm.config.error_fn {
                        // wren_c sends module = null, line = -1 in the
                        // first message we're sending the info for the
                        // top frame instead.
                        error_fn(
                            vm,
                            WrenErrorType::Runtime,
                            module,
                            e.stack_trace.frames[0].line,
                            &e.msg,
                        );
                        for frame in &e.stack_trace.frames {
                            error_fn(
                                vm,
                                WrenErrorType::StackTrace,
                                &frame.module,
                                frame.line,
                                &frame.fn_name,
                            );
                        }
                    }
                    WrenInterpretResult::RuntimeError
                }
                Ok(_) => WrenInterpretResult::Success,
            }
        }
    }
}
