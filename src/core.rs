use crate::vm::*;

// System.print is not actually in C in wren_c, but since we can't yet parse
// classes or methods, implementing here to get unit tests working.
pub(crate) fn prim_system_print(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    let value = args.pop().unwrap();
    let string = match &value {
        Value::Null => "null".into(),
        Value::Num(i) => format!("{}", i),
        Value::Boolean(b) => format!("{}", b),
        Value::String(s) => format!("{}", s),
        Value::Object(o) => format!("{:?}", o),
    };

    println!("{}", string);
    Ok(value)
}

fn num_plus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(
        args[0].try_into_num()? + args[1].try_into_num()?,
    ))
}
fn num_minus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(
        args[0].try_into_num()? - args[1].try_into_num()?,
    ))
}
fn num_unary_minus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(-args[0].try_into_num()?))
}
fn num_mult(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(
        args[0].try_into_num()? * args[1].try_into_num()?,
    ))
}
fn num_divide(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(
        args[0].try_into_num()? / args[1].try_into_num()?,
    ))
}
fn num_lt(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(
        args[0].try_into_num()? < args[1].try_into_num()?,
    ))
}
fn num_gt(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(
        args[0].try_into_num()? > args[1].try_into_num()?,
    ))
}
fn num_range_inclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let start = args[0].try_into_num()?;
    let end = args[1].try_into_num()?;
    Ok(Value::Object(wren_new_range(vm, start, end, true)))
}
fn num_range_exclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let start = args[0].try_into_num()?;
    let end = args[1].try_into_num()?;
    Ok(Value::Object(wren_new_range(vm, start, end, false)))
}

pub(crate) fn register_core_primitives(vm: &mut WrenVM) {
    // FIXME: This is TOO VERBOSE.  Fix this with macros maybe?
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "+(_)",
        num_plus,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "-(_)",
        num_minus,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "-",
        num_unary_minus,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "*(_)",
        num_mult,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "/(_)",
        num_divide,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "<(_)",
        num_lt,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        ">(_)",
        num_gt,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "..(_)",
        num_range_inclusive,
    );
    register_primitive(
        &mut vm.methods,
        &mut vm.num_class.borrow_mut(),
        "...(_)",
        num_range_exclusive,
    );
}
