// An attempt at defining a "rusty" API for Wren.  Definitely still needs work.
// This was originally used as both the C API and the Rust API, but should
// move away from any ties to needing to by c-like now that we have c_api.rs
// to implement the C-API on top of whatever rust API this exposes.

use vmgc::HandleScope;

pub use crate::vm::{SlotType, UserData, VM};

pub static VERSION_STRING: &str = "safe_wren-0.1";

// A function callable from Wren code, but implemented in another language.
// FIXME: How does this report errors?
pub type ForeignMethodFn = fn(vm: &mut VM);

// A finalizer function for freeing resources owned by an instance of a foreign
// class. Unlike most foreign methods, finalizers do not have access to the VM
// and should not interact with it since it's in the middle of a garbage
// collection.
// FIXME: a rusty data should be Box<dyn Trait> maybe?
pub type FinalizerFn = fn(data: *mut std::ffi::c_void);

// Gives the host a chance to canonicalize the imported module name,
// potentially taking into account the (previously resolved) name of the module
// that contains the import. Typically, this is used to implement relative
// imports.
pub type ResolveModuleFn = fn(vm: &VM, importer: &str, name: &str) -> String;

// The result of a loadModuleFn call.
// [source] is the source code for the module.
pub struct LoadModuleResult {
    pub source: String,
}

// Loads and returns the source code for the module [name] if found.
pub type LoadModuleFn = fn(vm: &VM, name: &str) -> Option<LoadModuleResult>;

// Returns a pointer to a foreign method on [className] in [module] with
// [signature].
pub type BindForeignMethodFn = fn(
    vm: &VM,
    module: &str,
    class_name: &str,
    is_static: bool,
    signature: &str,
) -> Option<ForeignMethodFn>;

// Displays a string of text to the user.
pub type WriteFn = fn(vm: &VM, text: &str);

pub enum ErrorType {
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
pub type ErrorFn = fn(vm: &VM, error_type: ErrorType, module: &str, line: usize, message: &str);

pub struct ForeignClassMethods {
    // The callback invoked when the foreign object is created.
    //
    // This must be provided. Inside the body of this, it must call
    // [wrenSetSlotNewForeign()] exactly once.
    pub allocate: ForeignMethodFn,
    // The callback invoked when the garbage collector is about to collect a
    // foreign object's memory.
    //
    // This may be `None` if the foreign class does not need to finalize.
    pub finalize: Option<FinalizerFn>,
}

// Returns a pair of pointers to the foreign methods used to allocate and
// finalize the data for instances of [className] in resolved [module].
pub type BindForeignClassFn =
    fn(vm: &VM, module_name: &str, class_name: &str) -> ForeignClassMethods;

pub struct Configuration {
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
    pub resolve_module_fn: Option<ResolveModuleFn>,

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
    pub load_module_fn: Option<LoadModuleFn>,

    // The callback Wren uses to find a foreign method and bind it to a class.
    //
    // When a foreign method is declared in a class, this will be called with the
    // foreign method's module, class, and signature when the class body is
    // executed. It should return a pointer to the foreign function that will be
    // bound to that method.
    //
    // If the foreign function could not be found, this should return NULL and
    // Wren will report it as runtime error.
    pub bind_foreign_method_fn: Option<BindForeignMethodFn>,

    // The callback Wren uses to find a foreign class and get its foreign methods.
    //
    // When a foreign class is declared, this will be called with the class's
    // module and name when the class body is executed. It should return the
    // foreign functions uses to allocate and (optionally) finalize the bytes
    // stored in the foreign object when an instance is created.
    pub bind_foreign_class_fn: Option<BindForeignClassFn>,

    // The callback Wren uses to display text when `System.print()` or the other
    // related functions are called.
    //
    // If this is `NULL`, Wren discards any printed text.
    pub write_fn: Option<WriteFn>,

    // The callback Wren uses to report errors.
    //
    // When an error occurs, this will be called with the module name, line
    // number, and an error message. If this is None, Wren doesn't report any
    // errors.
    pub error_fn: Option<ErrorFn>,

    // FIXME: Hack during development, shouldn't be API.
    pub debug_level: Option<DebugLevel>,

    // Unlike wren_c we're currently limiting max-heap size. Defaults to 50mb.
    pub heap_limit_bytes: usize,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            resolve_module_fn: None,
            load_module_fn: None,
            write_fn: None,
            bind_foreign_class_fn: None,
            error_fn: None,
            bind_foreign_method_fn: None,
            debug_level: None,
            heap_limit_bytes: 50 * 1024 * 1024,
        }
    }
}

#[allow(dead_code)]
pub enum DebugLevel {
    NonCore,
    All,
}

pub enum InterpretResult {
    Success,
    CompileError,
    RuntimeError,
}

// Runs [source], a string of Wren source code in a new fiber in [vm] in the
// context of resolved [module].
impl VM {
    // Should this take a &[u8]?
    pub fn interpret_bytes(
        &mut self,
        scope: &HandleScope,
        module: &str,
        source_bytes: Vec<u8>,
    ) -> InterpretResult {
        // Attempt to convert to String.
        match String::from_utf8(source_bytes) {
            Ok(source) => self.interpret(scope, module, source),
            Err(error) => {
                // Convert any unicode failures into compile failures.
                // Fake the line number according to the number of \n before the offset.
                let utf8_error = error.utf8_error();
                let source_bytes = error.as_bytes();
                let first_bad_byte = utf8_error.valid_up_to() + 1;
                let valid_bytes = &source_bytes[..first_bad_byte];
                let newline_count = valid_bytes.iter().filter(|&n| *n == b'\n').count();
                let line = newline_count + 1;
                let bad_byte = source_bytes[first_bad_byte];
                self.report_compile_error(
                    module,
                    line,
                    &format!("Error: Invalid byte 0x{:02x}.", bad_byte),
                );
                InterpretResult::CompileError
            }
        }
    }

    fn report_compile_error(&self, module: &str, line: usize, message: &str) {
        if let Some(error_fn) = self.config.error_fn {
            error_fn(self, ErrorType::Compile, module, line, message);
        }
    }

    // Should this take a &str?
    pub fn interpret(
        &mut self,
        scope: &HandleScope,
        module: &str,
        source: String,
    ) -> InterpretResult {
        match self.compile_source(&scope, module, source) {
            Err(error) => {
                self.report_compile_error(module, error.line, &error.error.to_string());
                InterpretResult::CompileError
            }
            Ok(closure) => {
                // wren_c does the fiber creation here instead.
                match self.run(&scope, closure) {
                    Err(e) => {
                        if let Some(error_fn) = self.config.error_fn {
                            // wren_c sends module = null, line = -1 in the
                            // first message we're sending the info for the
                            // top frame instead.
                            error_fn(
                                self,
                                ErrorType::Runtime,
                                module,
                                e.stack_trace.frames[0].line,
                                &e.msg,
                            );
                            for frame in &e.stack_trace.frames {
                                error_fn(
                                    self,
                                    ErrorType::StackTrace,
                                    &frame.module,
                                    frame.line,
                                    &frame.fn_name,
                                );
                            }
                        }
                        InterpretResult::RuntimeError
                    }
                    Ok(_) => InterpretResult::Success,
                }
            }
        }
    }
}

pub type Slot = usize;
