// analog to wren_core.c from wren_c.

use crate::vm::*;
use std::collections::VecDeque;
use std::ops::*;

type Result<T, E = VMError> = std::result::Result<T, E>;
type Handle<T> = std::rc::Rc<std::cell::RefCell<T>>;

// wren_c has these AS_RANGE, AS_CLASS, etc. macros
// which (unsafely) do direct "downcasts" to the type.
// These are our safer (and error-message sharing) alternatives.
// FIXME: Rust would expect "unwrap" in these names.
// e.g. unwrap_this_as_range
fn this_as_range(args: &Vec<Value>) -> Handle<ObjRange> {
    args[0].try_into_range().unwrap()
}
fn this_as_string(args: &Vec<Value>) -> String {
    args[0].try_into_string().unwrap()
}
fn this_as_closure(args: &Vec<Value>) -> Handle<ObjClosure> {
    args[0].try_into_closure().unwrap()
}
fn this_as_map(args: &Vec<Value>) -> Handle<ObjMap> {
    args[0].try_into_map().unwrap()
}
fn this_as_list(args: &Vec<Value>) -> Handle<ObjList> {
    args[0].try_into_list().unwrap()
}
fn this_as_class(args: &Vec<Value>) -> Handle<ObjClass> {
    args[0].try_into_class().unwrap()
}
fn this_as_fiber(args: &Vec<Value>) -> Handle<ObjFiber> {
    args[0].try_into_fiber().unwrap()
}

macro_rules! num_constant {
    ($func:ident, $value:expr) => {
        fn $func(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
            Ok(Value::Num($value))
        }
    };
}

fn validate_num(value: &Value, arg_name: &str) -> Result<f64> {
    value
        .try_into_num()
        .ok_or_else(|| VMError::from_string(format!("{} must be a number.", arg_name)))
}

macro_rules! infix_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            let b = validate_num(&args[1], "Right operand")?;
            Ok(Value::$return_type(a.$method(&b)))
        }
    };
}

// This is identical to infix_num_op except the borrow for b. :/
macro_rules! num_binary_op {
    ($func:ident, $method:ident, $return_type:ident, $msg:expr) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            let b = validate_num(&args[1], $msg)?;
            Ok(Value::$return_type(a.$method(b)))
        }
    };
}

macro_rules! bitwise_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            let b = validate_num(&args[1], "Right operand")? as u32;
            Ok(Value::from_u32(a.$method(&b)))
        }
    };
}

macro_rules! overflowing_bitwise_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            let b = validate_num(&args[1], "Right operand")? as u32;
            Ok(Value::from_u32(a.$method(b).0))
        }
    };
}

macro_rules! num_bitwise_unary_op {
    ($func:ident, $method:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            Ok(Value::from_u32(a.$method()))
        }
    };
}

macro_rules! num_unary_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            Ok(Value::$return_type(a.$method()))
        }
    };
}

num_constant!(num_infinity, f64::INFINITY);
num_constant!(num_nan, f64::NAN);
num_constant!(num_pi, 3.14159265358979323846264338327950288);
num_constant!(num_tau, 6.28318530717958647692528676655900577);
num_constant!(num_largest, f64::MAX);
num_constant!(num_smallest, f64::MIN);
num_constant!(num_max_safe_integer, 9007199254740991.0);
num_constant!(num_min_safe_integer, -9007199254740991.0);

infix_num_op!(num_plus, add, Num);
infix_num_op!(num_minus, sub, Num);
infix_num_op!(num_mult, mul, Num);
infix_num_op!(num_divide, div, Num);
infix_num_op!(num_lt, lt, Boolean);
infix_num_op!(num_gt, gt, Boolean);
infix_num_op!(num_lte, le, Boolean);
infix_num_op!(num_gte, ge, Boolean);

bitwise_num_op!(num_bitwise_and, bitand, Num);
bitwise_num_op!(num_bitwise_or, bitor, Num);
bitwise_num_op!(num_bitwise_xor, bitxor, Num);
overflowing_bitwise_num_op!(num_bitwise_shl, overflowing_shl, Num);
overflowing_bitwise_num_op!(num_bitwise_shr, overflowing_shr, Num);

fn num_range_inclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let start = validate_num(&args[0], "Left hand side of range")?;
    let end = validate_num(&args[1], "Right hand side of range")?;
    Ok(Value::Range(wren_new_range(vm, start, end, true)))
}
fn num_range_exclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let start = validate_num(&args[0], "Left hand side of range")?;
    let end = validate_num(&args[1], "Right hand side of range")?;
    Ok(Value::Range(wren_new_range(vm, start, end, false)))
}
num_binary_op!(num_atan2, atan2, Num, "x value");
num_binary_op!(num_pow, powf, Num, "Power value");
num_unary_op!(num_unary_minus, neg, Num);

fn num_fraction(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let a = validate_num(&args[0], "this")?;
    let fract = a.fract();
    // wren_c uses modf which seems to be negative if original is?
    // rust seems to only "get this wrong" for zero.
    if fract == 0.0 && a.is_sign_negative() {
        Ok(Value::Num(-0.0))
    } else {
        Ok(Value::Num(fract))
    }
}

num_unary_op!(num_is_infinity, is_infinite, Boolean);
num_unary_op!(num_is_nan, is_nan, Boolean);

fn num_sign(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    // wren_c Num.sign behavior differs from f64.signum at 0 and nan
    let a = validate_num(&args[0], "this")?;
    let sign = if a == 0.0 || a.is_nan() {
        0.0
    } else if a == -0.0 {
        -0.0
    } else {
        a.signum()
    };
    Ok(Value::Num(sign))
}

num_unary_op!(num_truncate, trunc, Num);

num_unary_op!(num_abs, abs, Num);
num_unary_op!(num_acos, acos, Num);
num_unary_op!(num_asin, asin, Num);
num_unary_op!(num_atan, atan, Num);
num_unary_op!(num_cbrt, cbrt, Num);
num_unary_op!(num_ceil, ceil, Num);
num_unary_op!(num_cos, cos, Num);
num_unary_op!(num_floor, floor, Num);

num_unary_op!(num_round, round, Num);
num_binary_op!(num_min, min, Num, "Other value");
num_binary_op!(num_max, max, Num, "Other value");

fn num_clamp(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let value = validate_num(&args[0], "this")?;
    let min = validate_num(&args[1], "Min value")?;
    let max = validate_num(&args[2], "Max value")?;
    Ok(Value::Num(value.clamp(min, max)))
}

num_unary_op!(num_sin, sin, Num);
num_unary_op!(num_sqrt, sqrt, Num);
num_unary_op!(num_tan, tan, Num);
num_unary_op!(num_log, ln, Num);
num_unary_op!(num_log2, log2, Num);
num_unary_op!(num_exp, exp, Num);
num_binary_op!(num_mod, rem, Num, "Right operand");
num_bitwise_unary_op!(num_bitwise_not, not);

fn num_is_integer(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let x = validate_num(&args[0], "this")?;
    Ok(Value::Boolean(
        !x.is_nan() && !x.is_infinite() && (x.trunc() == x),
    ))
}

fn wren_num_to_string(num: f64) -> String {
    // Wren prints nan vs NaN and inifity vs inf.
    if num.is_nan() {
        return "nan".into();
    }
    if num.is_infinite() {
        if num.is_sign_positive() {
            return "infinity".into();
        } else {
            return "-infinity".into();
        }
    }
    // Wren prints -0 differently from 0.
    // rust does sometime too?  But seems inconsistent about it.
    // https://github.com/rust-lang/rfcs/issues/1074
    if num == -0.0 && num.is_sign_negative() {
        return "-0".into();
    }

    // wren_c uses sprintf(buffer, "%.14g", value)
    // It's not clear how to set a maximum precision without also
    // forcing rust to use that precision always.
    // e.g. "{:.14}" would always print 14 digits.
    // See also https://github.com/rust-lang/rfcs/issues/844

    // Hacks to get us closer:
    let log_x = num.abs().log10();
    if (log_x >= -4.0 && log_x <= 14.0) || num == 0.0 {
        format!("{}", num)
    } else {
        let sci = format!("{:e}", num);
        // rust prints "1e10" rather than "1e+10" which wren_c expects.
        if num < 1.0 {
            sci
        } else {
            sci.replace("e", "e+")
        }
    }
}

fn num_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let value = validate_num(&args[0], "this")?;
    let string = wren_num_to_string(value);
    Ok(Value::from_string(string))
}

fn class_name(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_class(&args);
    let string = this.borrow().name.clone();
    Ok(Value::from_string(string))
}

fn class_supertype(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_class(&args);
    let maybe_superclass = &this.borrow().superclass;
    match maybe_superclass {
        None => Ok(Value::Null),
        Some(superclass) => Ok(Value::Class(superclass.clone())),
    }
}

fn class_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_class(&args);
    let string = this.borrow().name.clone();
    Ok(Value::from_string(string))
}

fn object_eqeq(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(args[0].eq(&args[1])))
}

// Note this is a static method, comparing two passed args.
fn object_same(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(args[1].eq(&args[2])))
}

fn object_bangeq(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(args[0].ne(&args[1])))
}

fn object_is(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let expected_baseclass = args[1]
        .try_into_class()
        .ok_or_else(|| VMError::from_str("Right operand must be a class."))?;
    // Start the class walk from class of "this".
    let mut class = vm.class_for_value(&args[0]);
    // Should this just be an iterator?
    // e.g. for class in object.class_chain()

    loop {
        if *expected_baseclass.borrow().name == *class.borrow().name {
            return Ok(Value::Boolean(true));
        }
        let superclass = match &class.borrow().superclass {
            Some(superclass) => superclass.clone(),
            None => {
                return Ok(Value::Boolean(false));
            }
        };
        class = superclass;
    }
}

fn object_to_string(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::from_string(format!(
        "instance of {}",
        vm.class_for_value(&args[0]).borrow().name
    )))
}

macro_rules! range_getter {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let range_cell = this_as_range(&args);
            let range = range_cell.borrow();
            Ok(Value::$return_type(range.$method))
        }
    };
}

// FIXME: Should be possible to share with range_getter?
macro_rules! range_getter_fn {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
            let range_cell = this_as_range(&args);
            let range = range_cell.borrow();
            Ok(Value::$return_type(range.$method()))
        }
    };
}

range_getter!(range_from, from, Num);
range_getter!(range_to, to, Num);
range_getter!(range_is_inclusive, is_inclusive, Boolean);
range_getter_fn!(range_min, min, Num);
range_getter_fn!(range_max, max, Num);

fn range_iterate(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let range_cell = this_as_range(&args);
    let range = range_cell.borrow();

    // Special case: empty range.
    if range.from == range.to && !range.is_inclusive {
        return Ok(Value::Boolean(false)); // No more elements.
    }
    // Start the iteration.
    if args[1].is_null() {
        return Ok(Value::Num(range.from));
    }

    let mut iterator = validate_num(&args[1], "Iterator")?;

    // Iterate towards [to] from [from].
    if range.from < range.to {
        iterator += 1.0;
        if iterator > range.to {
            return Ok(Value::Boolean(false));
        }
    } else {
        iterator -= 1.0;
        if iterator < range.to {
            return Ok(Value::Boolean(false));
        }
    }

    if !range.is_inclusive && iterator == range.to {
        return Ok(Value::Boolean(false));
    }

    Ok(Value::Num(iterator))
}

fn range_iterator_value(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    // Assuming args[1] is a number.
    Ok(args[1].clone())
}

fn range_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let range_ref = args[0]
        .try_into_range()
        .ok_or_else(|| VMError::from_str("this must be range"))?;
    let range = range_ref.borrow();

    let from = wren_num_to_string(range.from);
    let to = wren_num_to_string(range.to);
    let op = if range.is_inclusive { ".." } else { "..." };
    Ok(Value::from_string(format!("{}{}{}", from, op, to)))
}

fn object_type(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::Class(vm.class_for_value(&args[0])))
}

fn object_not(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(false))
}

fn bool_not(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(!args[0].equals_true()))
}

fn bool_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    if args[0].equals_true() {
        Ok(Value::from_str("true"))
    } else {
        Ok(Value::from_str("false"))
    }
}

fn fiber_new(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let closure = validate_fn(&args[1], "Argument")?;
    if closure.borrow().fn_obj.borrow().arity > 1 {
        Err(VMError::from_str(
            "Function cannot take more than one parameter.",
        ))
    } else {
        Ok(Value::Fiber(wren_new_fiber(vm, closure)))
    }
}

// This method sometimes causes a FiberAction (abort) and sometimes
// returns a value.  So for now it has to be a ValuePrimitive
// and use the Err result to cause the abort
fn fiber_abort(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let is_abort = !args[1].is_null();
    // wren_c just records the error string and continues?
    // vm->fiber->error = args[1];
    if is_abort {
        Err(VMError::FiberAbort(args[1].clone()))
    } else {
        // I guess Fiber.abort(null) clears the error?
        // wren_c: If the error is explicitly null, it's not really an abort.
        Ok(Value::Boolean(args[1].is_null()))
    }
}

fn fiber_current(vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::Fiber(vm.fiber.as_ref().unwrap().clone()))
}

fn fiber_suspend(_vm: &WrenVM, _args: Vec<Value>) -> Result<FiberAction> {
    Ok(FiberAction::Suspend)
}

fn fiber_yield(_vm: &WrenVM, _args: Vec<Value>) -> Result<FiberAction> {
    Ok(FiberAction::Return(Value::Null))
}

fn fiber_yield1(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    Ok(FiberAction::Return(args[1].clone()))
}

fn fiber_call(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "call")?;
    Ok(FiberAction::Call(this, Value::Null))
}

fn fiber_call1(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "call")?;
    Ok(FiberAction::Call(this, args[1].clone()))
}

fn fiber_transfer(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), false, "transfer to")?;
    Ok(FiberAction::Transfer(this, Value::Null))
}

fn fiber_transfer1(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), false, "transfer to")?;
    Ok(FiberAction::Transfer(this, args[1].clone()))
}

fn fiber_try(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "try")?;
    Ok(FiberAction::Try(this, Value::Null))
}

fn fiber_try1(_vm: &WrenVM, args: Vec<Value>) -> Result<FiberAction> {
    let this = this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "try")?;
    Ok(FiberAction::Try(this, args[1].clone()))
}

fn fiber_error(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_fiber(&args);
    let error = this.borrow().error();
    Ok(error)
}

fn fiber_is_done(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this_handle = this_as_fiber(&args);
    let this = this_handle.borrow();
    // We can't get a refernce to the (possibly currently running) stack
    // to check if empty, so use the completed_normally_cache.
    let is_done = this.has_error() || this.completed_normally_cache;
    Ok(Value::Boolean(is_done))
}

// Prepare to transfer execution to [fiber] coming from the current fiber whose
// stack has [args].
//
// [isCall] is true if [fiber] is being called and not transferred.
//
// [hasValue] is true if a value in [args] is being passed to the new fiber.
// Otherwise, `null` is implicitly being passed.
// This is called runFiber in wren_c.
fn validate_fiber_action(fiber: &ObjFiber, is_call: bool, verb: &str) -> Result<()> {
    if fiber.has_error() {
        return Err(VMError::from_string(format!(
            "Cannot {} an aborted fiber.",
            verb
        )));
    }

    if is_call {
        // You can't call a called fiber, but you can transfer directly to it,
        // which is why this check is gated on `isCall`. This way, after
        // resuming a suspended fiber, it will run and then return to the fiber
        // that called it and so on.
        if fiber.caller.is_some() {
            return Err(VMError::from_str("Fiber has already been called."));
        }

        if fiber.is_root() {
            return Err(VMError::from_str("Cannot call root fiber."));
        }
    }

    if fiber.completed_normally_cache {
        return Err(VMError::from_string(format!(
            "Cannot {} a finished fiber.",
            verb
        )));
    }
    Ok(())
}

fn null_not(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::Boolean(true))
}

fn null_to_string(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::from_str("null"))
}

fn validate_string(arg: &Value, arg_name: &str) -> Result<String> {
    arg.try_into_string()
        .ok_or_else(|| VMError::from_string(format!("{} must be a string.", arg_name)))
}

fn string_plus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let a = args[0]
        .try_into_string()
        .ok_or_else(|| VMError::from_str("this must be string"))?;
    let b = validate_string(&args[1], "Right operand")?;

    Ok(Value::from_string(a + &b))
}

fn string_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    // Do we need to confirm args[0] is a string?  wren_c does not.
    Ok(args[0].clone())
}

fn string_byte_count(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    Ok(Value::from_usize(this_as_string(&args).len()))
}

fn string_code_point_at(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let index = validate_index(&args[1], this.len(), "Index")?;

    // If we are in the middle of a UTF-8 sequence, indicate that.
    if !this.is_char_boundary(index) {
        return Ok(Value::Num(-1.0));
    }
    // FIXME: Might be a nicer way to do this in rust?
    let c = this.split_at(index).1.chars().nth(0).unwrap();
    Ok(Value::from_usize(c as usize))
}

fn string_byte_at(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let string = this_as_string(&args);
    let index = validate_index(&args[1], string.len(), "Index")?;
    Ok(Value::from_u8(string.as_bytes()[index]))
}

fn string_contains(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.contains(&search)))
}
fn string_ends_with(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.ends_with(&search)))
}

fn index_or_neg_one(maybe_index: Option<usize>) -> Value {
    match maybe_index {
        None => Value::Num(-1.0),
        Some(index) => Value::from_usize(index),
    }
}

fn string_index_of1(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    let maybe_index = this.find(&search);
    Ok(index_or_neg_one(maybe_index))
}

fn string_index_of2(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    let start = validate_index(&args[2], this.len(), "Start")?;
    // Rust will panic if you try to slice in the middle of a code point
    // since it's not possible to successfully "find" a partial code point, this
    // should be correct (and prevent the panic).
    if !this.is_char_boundary(start) {
        return Ok(Value::Num(-1.0));
    }
    let maybe_index = this[start..].find(&search);
    // This cannot use index_or_neg_one, due to adding start.
    match maybe_index {
        None => Ok(Value::Num(-1.0)),
        Some(index) => Ok(Value::from_usize(start + index)),
    }
}

fn string_starts_with(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let this = this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.starts_with(&search)))
}

fn validate_fn(arg: &Value, arg_name: &str) -> Result<Handle<ObjClosure>> {
    arg.try_into_closure()
        .ok_or_else(|| VMError::from_string(format!("{} must be a function.", arg_name)))
}

fn fn_new(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    // Odd that this never checks arg[0].
    // The block argument is already a function, so just return it.
    Ok(Value::Closure(validate_fn(&args[1], "Argument")?))
}

fn fn_arity(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let closure = this_as_closure(&args);
    let arity = closure.borrow().fn_obj.borrow().arity;
    Ok(Value::from_u8(arity))
}

fn fn_to_string(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::from_str("<fn>"))
}

fn map_new(vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::Map(wren_new_map(vm)))
}

fn map_subscript(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    validate_key(vm, &args[1])?;
    let map_cell = this_as_map(&args);
    let map = map_cell.borrow();
    let maybe_value = map.data.get(&args[1]);
    match maybe_value {
        None => Ok(Value::Null),
        Some(v) => Ok(v.clone()),
    }
}

fn map_subscript_setter(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    validate_key(vm, &args[1])?;
    let map = this_as_map(&args);
    map.borrow_mut()
        .data
        .insert(args[1].clone(), args[2].clone());
    Ok(args[2].clone())
}

fn wren_map_is_valid_key(arg: &Value) -> bool {
    match arg {
        Value::Boolean(_) => true,
        Value::Class(_) => true,
        Value::Null => true,
        Value::Num(_) => true,
        Value::Range(_) => true,
        Value::String(_) => true,
        _ => false,
    }
}

fn validate_key(_vm: &WrenVM, arg: &Value) -> Result<bool> {
    if wren_map_is_valid_key(arg) {
        Ok(true)
    } else {
        Err(VMError::from_str("Key must be a value type."))
    }
}

// Adds an entry to the map and then returns the map itself. This is called by
// the compiler when compiling map literals instead of using [_]=(_) to
// minimize stack churn.
fn map_add_core(vm: &WrenVM, mut args: Vec<Value>) -> Result<Value> {
    validate_key(vm, &args[1])?;

    let value = args.pop().unwrap();
    let key = args.pop().unwrap();
    let map = this_as_map(&args);
    map.borrow_mut().data.insert(key, value);
    // Return the map itself.
    Ok(Value::Map(map))
}

fn map_clear(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let map = this_as_map(&args);
    map.borrow_mut().data.clear();
    Ok(Value::Null)
}

fn map_contains_key(vm: &WrenVM, mut args: Vec<Value>) -> Result<Value> {
    validate_key(vm, &args[1])?;
    let map = this_as_map(&args);
    let key = args.pop().unwrap();
    let result = map.borrow().data.contains_key(&key);
    Ok(Value::Boolean(result))
}

fn map_count(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let map = this_as_map(&args);
    let count = map.borrow().data.len();
    Ok(Value::from_usize(count))
}

fn map_remove(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    validate_key(vm, &args[1])?;
    let map = this_as_map(&args);
    let maybe_value = map.borrow_mut().data.remove(&args[1]);
    match maybe_value {
        None => Ok(Value::Null),
        Some(value) => Ok(value),
    }
}

fn list_filled(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let size = validate_int(&args[1], "Size")?;
    if size < 0.0 {
        return Err(VMError::from_str("Size cannot be negative."));
    }
    let contents = vec![args[2].clone(); size as usize];
    Ok(Value::List(wren_new_list(vm, contents)))
}

fn list_new(vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    Ok(Value::List(wren_new_list(vm, Vec::new())))
}

struct Range {
    range: std::ops::RangeInclusive<usize>,
    reverse: bool,
}

impl Range {
    fn empty() -> Range {
        Range {
            range: 1..=0, // will be empty.
            reverse: false,
        }
    }
}

// FIXME: This could be much simpler!
fn calculate_range(range: &ObjRange, length: usize) -> Result<Range> {
    // Edge case: an empty range is allowed at the end of a sequence. This way,
    // list[0..-1] and list[0...list.count] can be used to copy a list even when
    // empty.
    let len_f = length as f64;
    let is_full_slice = if range.is_inclusive {
        range.from == len_f && range.to == -1.0
    } else {
        range.from == len_f && range.to == len_f
    };
    if is_full_slice {
        return Ok(Range::empty());
    }
    let from = validate_index_value(length, range.from as f64, "Range start")?;
    let from_f = from as f64;
    // Bounds check the end manually to handle exclusive ranges.
    let mut value = validate_int_value(range.to as f64, "Range end")?;
    // Negative indices count from the end.
    if value < 0.0 {
        value = len_f + value;
    }

    // Convert the exclusive range to an inclusive one.
    if !range.is_inclusive {
        // An exclusive range with the same start and end points is empty.
        if value == from_f {
            return Ok(Range::empty());
        }

        // Shift the endpoint to make it inclusive, handling both increasing and
        // decreasing ranges.
        if value >= from_f {
            value += -1.0;
        } else {
            value += 1.0;
        }
    }
    // Check bounds.
    if value < 0.0 || value >= len_f {
        return Err(VMError::from_str("Range end out of bounds."));
    }
    let to = value as usize;
    if from <= to {
        Ok(Range {
            range: from..=to,
            reverse: false,
        })
    } else {
        Ok(Range {
            range: to..=from,
            reverse: true,
        })
    }
}

fn list_subscript(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);

    match &args[1] {
        Value::Num(_) => {
            let index = validate_index(&args[1], list.borrow().elements.len(), "Subscript".into())?;
            Ok(list.borrow().elements[index].clone())
        }
        Value::Range(r) => {
            let range = calculate_range(&r.borrow(), list.borrow().elements.len())?;
            if range.range.is_empty() {
                return Ok(Value::List(wren_new_list(vm, Vec::new())));
            }
            let slice = &list.borrow().elements[range.range];
            let mut vec = slice.to_vec();
            if range.reverse {
                vec.reverse();
            }
            Ok(Value::List(wren_new_list(vm, vec)))
        }
        _ => Err(VMError::from_str("Subscript must be a number or a range.")),
    }
}

fn list_subscript_setter(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let index = validate_index(&args[1], list.borrow().elements.len(), "Subscript")?;
    list.borrow_mut().elements[index] = args[2].clone();
    Ok(args[2].clone())
}

fn list_add(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value> {
    let value = args.pop().unwrap();
    let list = this_as_list(&args);
    list.borrow_mut().elements.push(value.clone());
    Ok(value)
}

// Adds an element to the list and then returns the list itself. This is called
// by the compiler when compiling list literals instead of using add() to
// minimize stack churn.
fn list_add_core(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value> {
    let value = args.pop().unwrap();
    let list = this_as_list(&args);
    list.borrow_mut().elements.push(value);
    Ok(args[0].clone())
}

fn list_clear(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    list.borrow_mut().elements.clear();
    Ok(Value::Null)
}
fn list_insert(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let elements = &mut list.borrow_mut().elements;
    let count = elements.len();
    // count + 1 here so you can "insert" at the very end.
    let index = validate_index(&args[1], count + 1, "Index")?;
    if index == count {
        elements.push(args[2].clone());
    } else {
        elements.insert(index, args[2].clone());
    }
    Ok(args[2].clone())
}

fn list_remove_value(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let maybe_index = list.borrow().elements.iter().position(|v| v.eq(&args[1]));
    match maybe_index {
        None => Ok(Value::Null),
        Some(index) => {
            let value = list.borrow_mut().elements.remove(index);
            Ok(value)
        }
    }
}

fn list_index_of(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let maybe_index = list.borrow().elements.iter().position(|v| v.eq(&args[1]));
    Ok(index_or_neg_one(maybe_index))
}

fn list_swap(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let index_a = validate_index(&args[1], list.borrow().elements.len(), "Index 0")?;
    let index_b = validate_index(&args[2], list.borrow().elements.len(), "Index 1")?;

    list.borrow_mut().elements.swap(index_a, index_b);
    Ok(Value::Null)
}

fn validate_int_value(value: f64, arg_name: &str) -> Result<f64> {
    if value.trunc() != value {
        Err(VMError::from_string(format!(
            "{} must be an integer.",
            arg_name
        )))
    } else {
        Ok(value)
    }
}

fn validate_int(value: &Value, arg_name: &str) -> Result<f64> {
    let num = validate_num(value, arg_name)?;
    validate_int_value(num, arg_name)
}

fn list_iterate(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let elements_len = list.borrow().elements.len() as f64;

    if args[1].is_null() {
        if elements_len == 0.0 {
            return Ok(Value::Boolean(false));
        }
        return Ok(Value::Num(0.0));
    }

    let index = validate_int(&args[1], "Iterator")?;
    // Stop if we're out of bounds.
    if index < 0.0 || index >= elements_len - 1.0 {
        return Ok(Value::Boolean(false));
    }
    // Otherwise, move to the next index.
    return Ok(Value::Num(index + 1.0));
}

fn validate_index(value: &Value, count: usize, arg_name: &str) -> Result<usize> {
    let num = validate_num(value, arg_name)?;
    validate_index_value(count, num, arg_name)
}

// Validates that [value] is an integer within `[0, count)`. Also allows
// negative indices which map backwards from the end. Returns the valid positive
// index value. If invalid, reports an error.
fn validate_index_value(count: usize, mut value: f64, arg_name: &str) -> Result<usize> {
    validate_int_value(value, arg_name)?;

    // Negative indices count from the end.
    if value < 0.0 {
        value = count as f64 + value;
    }
    // Check bounds.
    if value >= 0.0 && value < count as f64 {
        Ok(value as usize)
    } else {
        Err(VMError::from_string(format!("{} out of bounds.", arg_name)))
    }
}

fn list_iterator_value(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);

    let index = validate_index(&args[1], list.borrow().elements.len(), "Iterator")?;
    let value = list.borrow().elements[index].clone();
    Ok(value)
}

fn list_remove_at(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let index = validate_index(&args[1], list.borrow().elements.len(), "Index")?;
    list.borrow_mut().elements.remove(index);
    Ok(Value::List(list))
}

fn list_count(_vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let list = this_as_list(&args);
    let count = list.borrow().elements.len();
    Ok(Value::from_usize(count))
}

// Modeled after https://github.com/saghm/unescape-rs
fn unescape(s: &str) -> String {
    let mut queue: VecDeque<_> = String::from(s).chars().collect();
    let mut s = String::new();

    while let Some(c) = queue.pop_front() {
        if c != '\\' {
            s.push(c);
            continue;
        }

        match queue.pop_front() {
            Some('n') => s.push('\n'),
            // Wren seems to intentionally handle \0 as a string terminator?
            Some('0') => return s,
            // Handle other escapes here if necessary.
            Some(c) => {
                s.push('\\');
                s.push(c);
            }
            None => {
                s.push('\\');
                return s;
            }
        };
    }
    s
}

fn system_clock(vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
    let now = std::time::Instant::now();
    Ok(Value::Num(now.duration_since(vm.start_time).as_secs_f64()))
}

// fn system_gc(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value> {
//     Ok(Value::Null)
// }

fn system_write_string(vm: &WrenVM, args: Vec<Value>) -> Result<Value> {
    let string = args[1]
        .try_into_string()
        .ok_or_else(|| VMError::from_str("expected String"))?;
    let result = unescape(&string);
    if let Some(write_fn) = vm.config.wren_write_fn {
        write_fn(vm, &result);
    }
    Ok(args[1].clone())
}

macro_rules! primitive {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let symbol = $vm.methods.ensure_symbol($sig);
        $class
            .borrow_mut()
            .set_method(symbol, Method::ValuePrimitive($func));
    };
}

macro_rules! fiber_primitive {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let symbol = $vm.methods.ensure_symbol($sig);
        $class
            .borrow_mut()
            .set_method(symbol, Method::FiberActionPrimitive($func));
    };
}

macro_rules! fiber_primitive_static {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let symbol = $vm.methods.ensure_symbol($sig);
        $class
            .borrow_mut()
            .class_obj()
            .borrow_mut()
            .set_method(symbol, Method::FiberActionPrimitive($func));
    };
}

macro_rules! primitive_static {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let symbol = $vm.methods.ensure_symbol($sig);
        $class
            .borrow_mut()
            .class_obj()
            .borrow_mut()
            .set_method(symbol, Method::ValuePrimitive($func));
    };
}

pub(crate) fn init_base_classes(vm: &mut WrenVM, core_module: &mut Module) {
    // wren_c makes a core module, which it then imports
    // into every module when running.  For now we're just
    // "importing" core directly into the one module we ever have.

    // FIXME: Store core_module in module map.
    // Define the root Object class. This has to be done a little specially
    // because it has no superclass.
    let object = define_class(core_module, "Object");
    primitive!(vm, object, "!", object_not);
    primitive!(vm, object, "==(_)", object_eqeq);
    primitive!(vm, object, "!=(_)", object_bangeq);
    primitive!(vm, object, "is(_)", object_is);
    primitive!(vm, object, "toString", object_to_string);
    primitive!(vm, object, "type", object_type);

    // Now we can define Class, which is a subclass of Object.
    let class = define_class(core_module, "Class");
    wren_bind_superclass(&mut class.borrow_mut(), &object);
    primitive!(vm, class, "name", class_name);
    primitive!(vm, class, "supertype", class_supertype);
    primitive!(vm, class, "toString", class_to_string);
    // primitive!(vm, class, "attributes", class_attributes);

    // Finally, we can define Object's metaclass which is a subclass of Class.
    let object_metaclass = define_class(core_module, "Object metaclass");
    // Wire up the metaclass relationships now that all three classes are built.
    object.borrow_mut().class = Some(object_metaclass.clone());
    object_metaclass.borrow_mut().class = Some(class.clone());
    class.borrow_mut().class = Some(class.clone());
    wren_bind_superclass(&mut object_metaclass.borrow_mut(), &class);

    primitive_static!(vm, object, "same(_,_)", object_same);

    // The core class diagram ends up looking like this, where single lines point
    // to a class's superclass, and double lines point to its metaclass:
    //
    //        .------------------------------------. .====.
    //        |                  .---------------. | #    #
    //        v                  |               v | v    #
    //   .---------.   .-------------------.   .-------.  #
    //   | Object  |==>| Object metaclass  |==>| Class |=="
    //   '---------'   '-------------------'   '-------'
    //        ^                                 ^ ^ ^ ^
    //        |                  .--------------' # | #
    //        |                  |                # | #
    //   .---------.   .-------------------.      # | # -.
    //   |  Base   |==>|  Base metaclass   |======" | #  |
    //   '---------'   '-------------------'        | #  |
    //        ^                                     | #  |
    //        |                  .------------------' #  | Example classes
    //        |                  |                    #  |
    //   .---------.   .-------------------.          #  |
    //   | Derived |==>| Derived metaclass |=========="  |
    //   '---------'   '-------------------'            -'
}

// Only used by init_fn_and_fiber
fn create_and_define_class(
    vm: &mut WrenVM,
    module: &mut Module,
    name: &str,
    superclass: &Handle<ObjClass>,
) -> Handle<ObjClass> {
    let class = wren_new_class(vm, &superclass, ClassSource::Internal, name.into()).unwrap();
    wren_define_variable(module, name, Value::Class(class.clone())).unwrap();
    class
}

// Only used for initing before loading wren_core.wren.
pub(crate) fn init_fn_and_fiber(vm: &mut WrenVM, module: &mut Module) {
    // wren_c compiles wren_core.wren with functions/closures with a
    // null class. Manually initialize classes before compiling wren_core.wren.
    let superclass = module.expect_class("Object");
    vm.fn_class = Some(create_and_define_class(vm, module, "Fn", &superclass));
    // The Fiber used to run wren_core for wren_c has a null class.
    vm.fiber_class = Some(create_and_define_class(vm, module, "Fiber", &superclass));
}

pub(crate) fn register_core_primitives(vm: &mut WrenVM) {
    // Note: All of these methods are bound *after* setup from
    // superclass, so any classes added to a superclass
    // WOULD NOT end up inherited into wren_core.wren subclasses.

    let module = vm.core_module.as_ref().unwrap().borrow();

    let core = CoreClasses {
        bool_class: module.expect_class("Bool"),
        num: module.expect_class("Num"),
        string: module.expect_class("String"),
        null: module.expect_class("Null"),
        range: module.expect_class("Range"),
        list: module.expect_class("List"),
        map: module.expect_class("Map"),
    };

    primitive!(vm, core.bool_class, "!", bool_not);
    primitive!(vm, core.bool_class, "toString", bool_to_string);

    let fiber = vm.fiber_class.as_ref().unwrap();
    primitive_static!(vm, fiber, "new(_)", fiber_new);
    primitive_static!(vm, fiber, "abort(_)", fiber_abort);
    primitive_static!(vm, fiber, "current", fiber_current);
    fiber_primitive_static!(vm, fiber, "suspend()", fiber_suspend);
    fiber_primitive_static!(vm, fiber, "yield()", fiber_yield);
    fiber_primitive_static!(vm, fiber, "yield(_)", fiber_yield1);
    fiber_primitive!(vm, fiber, "call()", fiber_call);
    fiber_primitive!(vm, fiber, "call(_)", fiber_call1);
    primitive!(vm, fiber, "error", fiber_error);
    primitive!(vm, fiber, "isDone", fiber_is_done);
    fiber_primitive!(vm, fiber, "transfer()", fiber_transfer);
    fiber_primitive!(vm, fiber, "transfer(_)", fiber_transfer1);
    // primitive!(vm, fiber, "transferError(_)", fiber_transfer_error);
    fiber_primitive!(vm, fiber, "try()", fiber_try);
    fiber_primitive!(vm, fiber, "try(_)", fiber_try1);

    let fn_class = vm.fn_class.as_ref().unwrap();
    primitive_static!(vm, fn_class, "new(_)", fn_new);
    primitive!(vm, fn_class, "arity", fn_arity);

    for arity in 0..=crate::vm::MAX_PARAMETERS {
        let name = if arity == 0 {
            format!("call()")
        } else {
            // arity=1 -> "call(_)", arity=2 -> "call(_,_)", etc.
            format!("call({}{})", "_,".repeat(arity - 1), "_")
        };
        let symbol = vm.methods.ensure_symbol(&name);
        fn_class
            .borrow_mut()
            .set_method(symbol, Method::FunctionCall);
    }

    primitive!(vm, fn_class, "toString", fn_to_string);

    primitive!(vm, core.null, "!", null_not);
    primitive!(vm, core.null, "toString", null_to_string);

    // primitive_static!(vm, core.num, "fromString(_)", num_from_string);
    primitive_static!(vm, core.num, "infinity", num_infinity);
    primitive_static!(vm, core.num, "nan", num_nan);
    primitive_static!(vm, core.num, "pi", num_pi);
    primitive_static!(vm, core.num, "tau", num_tau);
    primitive_static!(vm, core.num, "largest", num_largest);
    primitive_static!(vm, core.num, "smallest", num_smallest);
    primitive_static!(vm, core.num, "maxSafeInteger", num_max_safe_integer);
    primitive_static!(vm, core.num, "minSafeInteger", num_min_safe_integer);
    primitive!(vm, core.num, "-(_)", num_minus);
    primitive!(vm, core.num, "+(_)", num_plus);
    primitive!(vm, core.num, "*(_)", num_mult);
    primitive!(vm, core.num, "/(_)", num_divide);
    primitive!(vm, core.num, "<(_)", num_lt);
    primitive!(vm, core.num, ">(_)", num_gt);
    primitive!(vm, core.num, "<=(_)", num_lte);
    primitive!(vm, core.num, ">=(_)", num_gte);
    primitive!(vm, core.num, "&(_)", num_bitwise_and);
    primitive!(vm, core.num, "|(_)", num_bitwise_or);
    primitive!(vm, core.num, "^(_)", num_bitwise_xor);
    primitive!(vm, core.num, "<<(_)", num_bitwise_shl);
    primitive!(vm, core.num, ">>(_)", num_bitwise_shr);
    primitive!(vm, core.num, "abs", num_abs);
    primitive!(vm, core.num, "acos", num_acos);
    primitive!(vm, core.num, "asin", num_asin);
    primitive!(vm, core.num, "atan", num_atan);
    primitive!(vm, core.num, "cbrt", num_cbrt);
    primitive!(vm, core.num, "ceil", num_ceil);
    primitive!(vm, core.num, "cos", num_cos);
    primitive!(vm, core.num, "floor", num_floor);
    primitive!(vm, core.num, "-", num_unary_minus);
    primitive!(vm, core.num, "round", num_round);
    primitive!(vm, core.num, "min(_)", num_min);
    primitive!(vm, core.num, "max(_)", num_max);
    primitive!(vm, core.num, "clamp(_,_)", num_clamp);
    primitive!(vm, core.num, "sin", num_sin);
    primitive!(vm, core.num, "sqrt", num_sqrt);
    primitive!(vm, core.num, "tan", num_tan);
    primitive!(vm, core.num, "log", num_log);
    primitive!(vm, core.num, "log2", num_log2);
    primitive!(vm, core.num, "exp", num_exp);
    primitive!(vm, core.num, "%(_)", num_mod);
    primitive!(vm, core.num, "~", num_bitwise_not);
    primitive!(vm, core.num, "..(_)", num_range_inclusive);
    primitive!(vm, core.num, "...(_)", num_range_exclusive);
    primitive!(vm, core.num, "atan(_)", num_atan2);
    primitive!(vm, core.num, "pow(_)", num_pow);
    primitive!(vm, core.num, "fraction", num_fraction);
    primitive!(vm, core.num, "isInfinity", num_is_infinity);
    primitive!(vm, core.num, "isInteger", num_is_integer);
    primitive!(vm, core.num, "isNan", num_is_nan);
    primitive!(vm, core.num, "sign", num_sign);
    primitive!(vm, core.num, "toString", num_to_string);
    primitive!(vm, core.num, "truncate", num_truncate);

    // These are defined just so that 0 and -0 are equal, which is specified by
    // IEEE 754 even though they have different bit representations.
    //   PRIMITIVE(vm->numClass, "==(_)", num_eqeq);
    //   PRIMITIVE(vm->numClass, "!=(_)", num_bangeq);

    // primitive_static!(vm, core.string, "fromCodePoint(_)", string_from_code_point);
    // primitive_static!(vm, core.string, "fromByte(_)", string_from_byte);
    primitive!(vm, core.string, "+(_)", string_plus);
    // primitive!(vm, core.string, "[_]", string_subscript);
    primitive!(vm, core.string, "byteAt_(_)", string_byte_at);
    primitive!(vm, core.string, "byteCount_", string_byte_count);
    primitive!(vm, core.string, "codePointAt_(_)", string_code_point_at);
    primitive!(vm, core.string, "contains(_)", string_contains);
    primitive!(vm, core.string, "endsWith(_)", string_ends_with);
    primitive!(vm, core.string, "indexOf(_)", string_index_of1);
    primitive!(vm, core.string, "indexOf(_,_)", string_index_of2);
    // primitive!(vm, core.string, "iterate(_)", string_iterate);
    // primitive!(vm, core.string, "iterateByte_(_)", string_iterate_byte);
    // primitive!(vm, core.string, "iteratorValue(_)", string_iterator_value);
    primitive!(vm, core.string, "startsWith(_)", string_starts_with);
    primitive!(vm, core.string, "toString", string_to_string);

    let list = module.expect_class("List");
    primitive_static!(vm, list, "filled(_,_)", list_filled);
    primitive_static!(vm, list, "new()", list_new);
    primitive!(vm, list, "[_]", list_subscript);
    primitive!(vm, list, "[_]=(_)", list_subscript_setter);
    primitive!(vm, list, "add(_)", list_add);
    primitive!(vm, list, "addCore_(_)", list_add_core);
    primitive!(vm, list, "clear()", list_clear);
    primitive!(vm, list, "count", list_count);
    primitive!(vm, list, "insert(_,_)", list_insert);
    primitive!(vm, list, "iterate(_)", list_iterate);
    primitive!(vm, list, "iteratorValue(_)", list_iterator_value);
    primitive!(vm, list, "removeAt(_)", list_remove_at);
    primitive!(vm, list, "remove(_)", list_remove_value);
    primitive!(vm, list, "indexOf(_)", list_index_of);
    primitive!(vm, list, "swap(_,_)", list_swap);

    let map = module.expect_class("Map");
    primitive_static!(vm, map, "new()", map_new);
    primitive!(vm, map, "[_]", map_subscript);
    primitive!(vm, map, "[_]=(_)", map_subscript_setter);
    primitive!(vm, map, "addCore_(_,_)", map_add_core);
    primitive!(vm, map, "clear()", map_clear);
    primitive!(vm, map, "containsKey(_)", map_contains_key);
    primitive!(vm, map, "count", map_count);
    primitive!(vm, map, "remove(_)", map_remove);
    // primitive!(vm, map, "iterate(_)", map_iterate);
    // primitive!(vm, map, "keyIteratorValue_(_)", map_key_iterator_value);
    // primitive!(vm, map, "valueIteratorValue_(_)", map_value_iterator_value);

    primitive!(vm, core.range, "from", range_from);
    primitive!(vm, core.range, "to", range_to);
    primitive!(vm, core.range, "min", range_min);
    primitive!(vm, core.range, "max", range_max);
    primitive!(vm, core.range, "isInclusive", range_is_inclusive);
    primitive!(vm, core.range, "iterate(_)", range_iterate);
    primitive!(vm, core.range, "iteratorValue(_)", range_iterator_value);
    primitive!(vm, core.range, "toString", range_to_string);

    let system = module.expect_class("System");
    primitive_static!(vm, system, "clock", system_clock);
    // primitive_static!(vm, system, "gc()", system_gc);
    primitive_static!(vm, system, "writeString_(_)", system_write_string);

    vm.core = Some(core);

    // wren_c walks *all* String objects here to bind a class pointer
    // to them.  We don't currently need to do that, but may need
    // to do that eventually for other types.
}
