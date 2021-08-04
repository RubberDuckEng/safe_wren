use wren_rust::wren::{ForeignMethodFn, VM};

fn runtime_error(vm: &mut VM) {
    vm.ensure_slots(1);
    vm.set_slot_string(0, "Error!");
    vm.abort_fiber(0);
}

pub fn error_bind_method(signature: &str) -> Option<ForeignMethodFn> {
    if signature == "static Error.runtimeError" {
        Some(runtime_error)
    } else {
        None
    }
}
