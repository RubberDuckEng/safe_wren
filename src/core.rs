// analog to wren_core.c from wren_c.

use crate::{float_to_string, vm::*};
use std::collections::VecDeque;
use std::ops::*;

type Result<T, E = VMError> = std::result::Result<T, E>;
type Handle<T> = std::rc::Rc<std::cell::RefCell<T>>;

// wren_c has these AS_RANGE, AS_CLASS, etc. macros
// which (unsafely) do direct "downcasts" to the type.
// These are our safer (and error-message sharing) alternatives.
fn unwrap_this_as_range(args: &[Value]) -> Handle<ObjRange> {
    args[0].try_into_range().unwrap()
}
fn unwrap_this_as_string(args: &[Value]) -> String {
    args[0].try_into_string().unwrap()
}
fn unwrap_this_as_closure(args: &[Value]) -> Handle<ObjClosure> {
    args[0].try_into_closure().unwrap()
}
fn unwrap_this_as_map(args: &[Value]) -> Handle<ObjMap> {
    args[0].try_into_map().unwrap()
}
fn unwrap_this_as_list(args: &[Value]) -> Handle<ObjList> {
    args[0].try_into_list().unwrap()
}
fn unwrap_this_as_class(args: &[Value]) -> Handle<ObjClass> {
    args[0].try_into_class().unwrap()
}
fn unwrap_this_as_fiber(args: &[Value]) -> Handle<ObjFiber> {
    args[0].try_into_fiber().unwrap()
}

macro_rules! num_constant {
    ($func:ident, $value:expr) => {
        fn $func(_vm: &VM, _args: &[Value]) -> Result<Value> {
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
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            let b = validate_num(&args[1], "Right operand")?;
            Ok(Value::$return_type(a.$method(&b)))
        }
    };
}

// This is identical to infix_num_op except the borrow for b. :/
macro_rules! num_binary_op {
    ($func:ident, $method:ident, $return_type:ident, $msg:expr) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            let b = validate_num(&args[1], $msg)?;
            Ok(Value::$return_type(a.$method(b)))
        }
    };
}

macro_rules! bitwise_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            let b = validate_num(&args[1], "Right operand")? as u32;
            Ok(Value::from_u32(a.$method(&b)))
        }
    };
}

macro_rules! overflowing_bitwise_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            let b = validate_num(&args[1], "Right operand")? as u32;
            Ok(Value::from_u32(a.$method(b).0))
        }
    };
}

macro_rules! num_bitwise_unary_op {
    ($func:ident, $method:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")? as u32;
            Ok(Value::from_u32(a.$method()))
        }
    };
}

macro_rules! num_unary_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let a = validate_num(&args[0], "this")?;
            Ok(Value::$return_type(a.$method()))
        }
    };
}

num_constant!(num_infinity, f64::INFINITY);
num_constant!(num_nan, f64::NAN);
num_constant!(num_pi, std::f64::consts::PI);
num_constant!(num_tau, std::f64::consts::TAU);
num_constant!(num_largest, f64::MAX);
num_constant!(num_smallest, f64::MIN_POSITIVE);
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

fn num_range_inclusive(vm: &VM, args: &[Value]) -> Result<Value> {
    let start = validate_num(&args[0], "Left hand side of range")?;
    let end = validate_num(&args[1], "Right hand side of range")?;
    Ok(Value::Range(vm.new_range(start, end, true)))
}
fn num_range_exclusive(vm: &VM, args: &[Value]) -> Result<Value> {
    let start = validate_num(&args[0], "Left hand side of range")?;
    let end = validate_num(&args[1], "Right hand side of range")?;
    Ok(Value::Range(vm.new_range(start, end, false)))
}
num_binary_op!(num_atan2, atan2, Num, "x value");
num_binary_op!(num_pow, powf, Num, "Power value");
num_unary_op!(num_unary_minus, neg, Num);

fn num_from_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = validate_string(&args[1], "Argument")?;

    // Corner case: Can't parse an empty string.
    if string.is_empty() {
        return Ok(Value::Null);
    }
    // FIXME: This accepts more than wren_c does (uses strtod).
    match string.trim().parse::<f64>() {
        Ok(num) => {
            if num.is_finite() {
                Ok(Value::Num(num))
            } else {
                Err(VMError::from_str("Number literal is too large."))
            }
        }
        Err(_) => Ok(Value::Null),
    }
}

fn num_fraction(_vm: &VM, args: &[Value]) -> Result<Value> {
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

fn num_sign(_vm: &VM, args: &[Value]) -> Result<Value> {
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

fn num_clamp(_vm: &VM, args: &[Value]) -> Result<Value> {
    let value = validate_num(&args[0], "this")?;
    let min = validate_num(&args[1], "Min value")?;
    let max = validate_num(&args[2], "Max value")?;

    // Rust's f64::clamp panic's if min > max.
    let result = if value < min {
        min
    } else {
        if value > max {
            max
        } else {
            value
        }
    };
    Ok(Value::Num(result))
}

num_unary_op!(num_sin, sin, Num);
num_unary_op!(num_sqrt, sqrt, Num);
num_unary_op!(num_tan, tan, Num);
num_unary_op!(num_log, ln, Num);
num_unary_op!(num_log2, log2, Num);
num_unary_op!(num_exp, exp, Num);
num_binary_op!(num_mod, rem, Num, "Right operand");
num_bitwise_unary_op!(num_bitwise_not, not);

// I don't know of a better way to check this.
#[allow(clippy::float_cmp)]
fn f64_is_integer(x: f64) -> bool {
    x.trunc() == x
}

fn num_is_integer(_vm: &VM, args: &[Value]) -> Result<Value> {
    let x = validate_num(&args[0], "this")?;
    Ok(Value::Boolean(
        !x.is_nan() && !x.is_infinite() && f64_is_integer(x),
    ))
}

fn wren_num_to_string(num: f64) -> String {
    // Wren prints nan vs NaN and infinity vs inf.
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

    // wren_c uses sprintf(buffer, "%.14g", value).  Rust doesn't
    // support %g, so we had to write our own.
    // See also https://github.com/rust-lang/rfcs/issues/844
    float_to_string::float_to_shortest_string(num, 14)
}

fn num_to_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    let value = validate_num(&args[0], "this")?;
    let string = wren_num_to_string(value);
    Ok(Value::from_string(string))
}

fn class_name(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_class(&args);
    let string = this.borrow().name.clone();
    Ok(Value::from_string(string))
}

fn class_supertype(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_class(&args);
    let maybe_superclass = &this.borrow().superclass;
    match maybe_superclass {
        None => Ok(Value::Null),
        Some(superclass) => Ok(Value::Class(superclass.clone())),
    }
}

fn class_to_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_class(&args);
    let string = this.borrow().name.clone();
    Ok(Value::from_string(string))
}

fn object_eqeq(_vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(args[0].eq(&args[1])))
}

// Note this is a static method, comparing two passed args.
fn object_same(_vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(args[1].eq(&args[2])))
}

fn object_bangeq(_vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(args[0].ne(&args[1])))
}

fn object_is(vm: &VM, args: &[Value]) -> Result<Value> {
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

fn object_to_string(vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::from_string(format!(
        "instance of {}",
        vm.class_for_value(&args[0]).borrow().name
    )))
}

macro_rules! range_getter {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let range_cell = unwrap_this_as_range(&args);
            let range = range_cell.borrow();
            Ok(Value::$return_type(range.$method))
        }
    };
}

// FIXME: Should be possible to share with range_getter?
macro_rules! range_getter_fn {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &VM, args: &[Value]) -> Result<Value> {
            let range_cell = unwrap_this_as_range(&args);
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

fn range_iterate(_vm: &VM, args: &[Value]) -> Result<Value> {
    let range_cell = unwrap_this_as_range(&args);
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

fn range_iterator_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    // Assuming args[1] is a number.
    Ok(args[1].clone())
}

fn range_to_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    let range_ref = args[0]
        .try_into_range()
        .ok_or_else(|| VMError::from_str("this must be range"))?;
    let range = range_ref.borrow();

    let from = wren_num_to_string(range.from);
    let to = wren_num_to_string(range.to);
    let op = if range.is_inclusive { ".." } else { "..." };
    Ok(Value::from_string(format!("{}{}{}", from, op, to)))
}

fn object_type(vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::Class(vm.class_for_value(&args[0])))
}

fn object_not(_vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(false))
}

fn bool_not(_vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(!args[0].equals_true()))
}

fn bool_to_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    if args[0].equals_true() {
        Ok(Value::from_str("true"))
    } else {
        Ok(Value::from_str("false"))
    }
}

fn fiber_new(vm: &VM, args: &[Value]) -> Result<Value> {
    let closure = validate_fn(&args[1], "Argument")?;
    if closure.borrow().fn_obj.borrow().arity.as_index() > 1 {
        Err(VMError::from_str(
            "Function cannot take more than one parameter.",
        ))
    } else {
        Ok(Value::Fiber(vm.new_fiber(closure)))
    }
}

// This method sometimes causes a FiberAction (abort) and sometimes
// returns a value.  So for now it has to be a ValuePrimitive
// and use the Err result to cause the abort
fn fiber_abort(_vm: &VM, args: &[Value]) -> Result<Value> {
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

fn fiber_current(vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::Fiber(vm.fiber.as_ref().unwrap().clone()))
}

fn fiber_suspend(_vm: &VM, _args: &[Value]) -> Result<FiberAction> {
    Ok(FiberAction::Suspend)
}

fn fiber_yield(_vm: &VM, _args: &[Value]) -> Result<FiberAction> {
    Ok(FiberAction::Return(Value::Null))
}

fn fiber_yield1(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    Ok(FiberAction::Return(args[1].clone()))
}

fn fiber_call(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "call")?;
    Ok(FiberAction::Call(this, Value::Null))
}

fn fiber_call1(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "call")?;
    Ok(FiberAction::Call(this, args[1].clone()))
}

fn fiber_transfer(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), false, "transfer to")?;
    Ok(FiberAction::Transfer(this, Value::Null))
}

fn fiber_transfer1(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), false, "transfer to")?;
    Ok(FiberAction::Transfer(this, args[1].clone()))
}

fn fiber_transfer_error(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), false, "transfer to")?;
    Ok(FiberAction::TransferError(this, args[1].clone()))
}

fn fiber_try(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "try")?;
    Ok(FiberAction::Try(this, Value::Null))
}

fn fiber_try1(_vm: &VM, args: &[Value]) -> Result<FiberAction> {
    let this = unwrap_this_as_fiber(&args);
    validate_fiber_action(&this.borrow(), true, "try")?;
    Ok(FiberAction::Try(this, args[1].clone()))
}

fn fiber_error(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_fiber(&args);
    let error = this.borrow().error();
    Ok(error)
}

fn fiber_is_done(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this_handle = unwrap_this_as_fiber(&args);
    let this = this_handle.borrow();
    // We can't get a refernce to the (possibly currently running) stack
    // to check if empty, so use the completed_normally_cache.
    let is_done = this.has_error() || this.completed_normally_cache;
    Ok(Value::Boolean(is_done))
}

// Prepare to transfer execution to [fiber] coming from the current fiber.
//
// [is_call] is true if [fiber] is being called and not transferred.
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

fn null_not(_vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(true))
}

fn null_to_string(_vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::from_str("null"))
}

fn validate_string(arg: &Value, arg_name: &str) -> Result<String> {
    arg.try_into_string()
        .ok_or_else(|| VMError::from_string(format!("{} must be a string.", arg_name)))
}

fn string_from_code_point(_vm: &VM, args: &[Value]) -> Result<Value> {
    let num = validate_int(&args[1], "Code point")?;
    if num < 0.0 {
        Err(VMError::from_str("Code point cannot be negative."))
    } else if (num as u32) > 0x10ffff {
        Err(VMError::from_str(
            "Code point cannot be greater than 0x10ffff.",
        ))
    } else {
        let c = char::from_u32(num as u32)
            .ok_or_else(|| VMError::from_str("Code point must be valid unicode"))?;
        Ok(Value::from_string(c.to_string()))
    }
}

fn string_from_byte(_vm: &VM, args: &[Value]) -> Result<Value> {
    let num = validate_int(&args[1], "Byte")?;
    if num < 0.0 {
        Err(VMError::from_str("Byte cannot be negative."))
    } else if (num as u32) > 0xff {
        Err(VMError::from_str("Byte cannot be greater than 0xff."))
    } else {
        let bytes = vec![num as u8];
        let string =
            String::from_utf8(bytes).map_err(|_| VMError::from_str("Byte must be valid utf8"))?;
        Ok(Value::from_string(string))
    }
}

fn string_plus(_vm: &VM, args: &[Value]) -> Result<Value> {
    let a = unwrap_this_as_string(&args);
    let b = validate_string(&args[1], "Right operand")?;
    Ok(Value::from_string(a + &b))
}

fn adjust_range_to_char_boundaries(
    string: &str,
    range: std::ops::RangeInclusive<usize>,
) -> std::ops::Range<usize> {
    let mut before = *range.start();
    let mut after = range.end() + 1;
    // wren_c only includes sequences whose first byte is in the range.
    while before <= after && !string.is_char_boundary(before) {
        before += 1;
    }
    while after < string.len() && !string.is_char_boundary(after) {
        after += 1;
    }
    // Intentionally converting to exclusive to avoid -1
    before..after
}

fn string_subscript(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = unwrap_this_as_string(&args);

    match &args[1] {
        Value::Num(_) => {
            let index = validate_index(&args[1], string.len(), "Subscript")?;
            Ok(wren_string_code_point_at(string, index))
        }
        Value::Range(r) => {
            let range = calculate_range(&r.borrow(), string.len())?;
            if range.range.is_empty() {
                return Ok(Value::from_str(""));
            }
            let safe_range = adjust_range_to_char_boundaries(&string, range.range);
            let slice = &string[safe_range];
            // This doesn't match the wren_c backwards range behavaior.
            if range.reverse {
                Ok(Value::from_string(slice.chars().rev().collect()))
            } else {
                Ok(Value::from_str(slice))
            }
        }
        _ => Err(VMError::from_str("Subscript must be a number or a range.")),
    }
}

fn string_to_string(_vm: &VM, args: &[Value]) -> Result<Value> {
    // Do we need to confirm args[0] is a string?  wren_c does not.
    Ok(args[0].clone())
}

fn string_byte_count(_vm: &VM, args: &[Value]) -> Result<Value> {
    Ok(Value::from_usize(unwrap_this_as_string(&args).len()))
}

fn string_code_point_at(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let index = validate_index(&args[1], this.len(), "Index")?;

    // If we are in the middle of a UTF-8 sequence, indicate that.
    if !this.is_char_boundary(index) {
        return Ok(Value::Num(-1.0));
    }
    // FIXME: Might be a nicer way to do this in rust?
    let c = this.split_at(index).1.chars().next().unwrap();
    Ok(Value::from_usize(c as usize))
}

fn string_byte_at(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = unwrap_this_as_string(&args);
    let index = validate_index(&args[1], string.len(), "Index")?;
    Ok(Value::from_u8(string.as_bytes()[index]))
}

fn string_contains(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.contains(&search)))
}
fn string_ends_with(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.ends_with(&search)))
}

fn index_or_neg_one(maybe_index: Option<usize>) -> Value {
    match maybe_index {
        None => Value::Num(-1.0),
        Some(index) => Value::from_usize(index),
    }
}

fn string_index_of1(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    let maybe_index = this.find(&search);
    Ok(index_or_neg_one(maybe_index))
}

fn string_index_of2(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    let mut start = validate_index(&args[2], this.len(), "Start")?;
    // Rust will panic if you try to slice in the middle of a code point.
    // Since it's not possible to "find" a partial codepoint we just
    // adjust start to the next valid char_boundary and search there.
    while start < this.len() && !this.is_char_boundary(start) {
        start += 1;
    }
    let maybe_index = this[start..].find(&search);
    // This cannot use index_or_neg_one, due to adding start.
    match maybe_index {
        None => Ok(Value::Num(-1.0)),
        Some(index) => Ok(Value::from_usize(start + index)),
    }
}

fn string_iterate(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = unwrap_this_as_string(&args);

    // If we're starting the iteration, return the first index.
    if args[1].is_null() {
        return Ok(if string.is_empty() {
            Value::Boolean(false)
        } else {
            Value::Num(0.0)
        });
    }

    let num = validate_int(&args[1], "Iterator")?;
    if num < 0.0 {
        return Ok(Value::Boolean(false));
    }
    let mut index = num as usize;

    // Advance to the beginning of the next UTF-8 sequence.
    loop {
        index += 1;
        if index >= string.len() {
            return Ok(Value::Boolean(false));
        }
        if string.is_char_boundary(index) {
            return Ok(Value::from_usize(index));
        }
    }
}

fn string_iterate_byte(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = unwrap_this_as_string(&args);

    // If we're starting the iteration, return the first index.
    if args[1].is_null() {
        return Ok(if string.is_empty() {
            Value::Boolean(false)
        } else {
            Value::Num(0.0)
        });
    }

    let num = validate_int(&args[1], "Iterator")?;
    if num < 0.0 {
        return Ok(Value::Boolean(false));
    }

    // Advance to the next byte.
    let index = num as usize + 1;
    Ok(if index >= string.len() {
        Value::Boolean(false)
    } else {
        Value::from_usize(index)
    })
}

fn wren_string_code_point_at(string: String, index: usize) -> Value {
    let mut chars = string.char_indices();
    let mut previous: char = char::default();
    // FIXME: This does not implement the "return an invalid byte"
    // behavior, since that's incompatible with using rust Strings
    // as storage.
    loop {
        if let Some((start, c)) = chars.next() {
            if start <= index {
                previous = c;
                continue;
            }
        }
        return Value::from_string(previous.to_string());
    }
}

fn string_iterator_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    let string = unwrap_this_as_string(&args);
    let index = validate_index(&args[1], string.len(), "Iterator")?;

    Ok(wren_string_code_point_at(string, index))
}

fn string_starts_with(_vm: &VM, args: &[Value]) -> Result<Value> {
    let this = unwrap_this_as_string(&args);
    let search = validate_string(&args[1], "Argument")?;
    Ok(Value::Boolean(this.starts_with(&search)))
}

fn validate_fn(arg: &Value, arg_name: &str) -> Result<Handle<ObjClosure>> {
    arg.try_into_closure()
        .ok_or_else(|| VMError::from_string(format!("{} must be a function.", arg_name)))
}

fn fn_new(_vm: &VM, args: &[Value]) -> Result<Value> {
    // Odd that this never checks arg[0].
    // The block argument is already a function, so just return it.
    Ok(Value::Closure(validate_fn(&args[1], "Argument")?))
}

fn fn_arity(_vm: &VM, args: &[Value]) -> Result<Value> {
    let closure = unwrap_this_as_closure(&args);
    let arity = closure.borrow().fn_obj.borrow().arity;
    Ok(Value::from_usize(arity.as_index()))
}

fn fn_to_string(_vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::from_str("<fn>"))
}

fn map_new(vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::Map(vm.new_map()))
}

fn map_subscript(vm: &VM, args: &[Value]) -> Result<Value> {
    let key = &args[1];
    validate_key(vm, key)?;
    let map_cell = unwrap_this_as_map(&args);
    let map = map_cell.borrow();
    let maybe_value = map.data.get(key);
    match maybe_value {
        None => Ok(Value::Null),
        Some(v) => Ok(v.clone()),
    }
}

fn map_subscript_setter(vm: &VM, args: &[Value]) -> Result<Value> {
    let key = &args[1];
    validate_key(vm, key)?;
    let map = unwrap_this_as_map(&args);
    let value = &args[2];
    map.borrow_mut().data.insert(key.clone(), value.clone());
    Ok(value.clone())
}

fn map_is_valid_key(arg: &Value) -> bool {
    matches!(
        arg,
        Value::Boolean(_)
            | Value::Class(_)
            | Value::Null
            | Value::Num(_)
            | Value::Range(_)
            | Value::String(_)
    )
}

fn validate_key(_vm: &VM, arg: &Value) -> Result<bool> {
    if map_is_valid_key(arg) {
        Ok(true)
    } else {
        Err(VMError::from_str("Key must be a value type."))
    }
}

// Adds an entry to the map and then returns the map itself. This is called by
// the compiler when compiling map literals instead of using [_]=(_) to
// minimize stack churn.
fn map_add_core(vm: &VM, args: &[Value]) -> Result<Value> {
    let key = &args[1];
    validate_key(vm, key)?;
    let value = &args[2];
    let map = unwrap_this_as_map(&args);
    map.borrow_mut().data.insert(key.clone(), value.clone());
    // Return the map itself.
    Ok(Value::Map(map))
}

fn map_clear(_vm: &VM, args: &[Value]) -> Result<Value> {
    let map = unwrap_this_as_map(&args);
    map.borrow_mut().data.clear();
    Ok(Value::Null)
}

fn map_contains_key(vm: &VM, args: &[Value]) -> Result<Value> {
    let key = &args[1];
    validate_key(vm, key)?;
    let map = unwrap_this_as_map(&args);
    let result = map.borrow().contains_key(key);
    Ok(Value::Boolean(result))
}

fn map_count(_vm: &VM, args: &[Value]) -> Result<Value> {
    let map = unwrap_this_as_map(&args);
    let count = map.borrow().len();
    Ok(Value::from_usize(count))
}

fn map_remove(vm: &VM, args: &[Value]) -> Result<Value> {
    let key = &args[1];
    validate_key(vm, key)?;
    let map = unwrap_this_as_map(&args);
    let maybe_value = map.borrow_mut().data.remove(key);
    match maybe_value {
        None => Ok(Value::Null),
        Some(value) => Ok(value),
    }
}

// FIXME: This is wrong.  This sits on top of rust's hashmap
// and does not match wren_c's iterator behavior exactly.
fn map_iterate(_vm: &VM, args: &[Value]) -> Result<Value> {
    let map = unwrap_this_as_map(&args);
    if map.borrow().data.is_empty() {
        return Ok(Value::Boolean(false));
    }

    if !args[1].is_null() {
        let value = validate_int(&args[1], "Iterator")?;
        if value < 0.0 {
            return Ok(Value::Boolean(false));
        }

        // Advance the iterator.
        let index = value as usize + 1;
        if index < map.borrow().len() {
            Ok(Value::from_usize(index))
        } else {
            Ok(Value::Boolean(false))
        }
    } else {
        Ok(Value::from_usize(0))
    }
}

fn map_key_iterator_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    let map_handle = unwrap_this_as_map(&args);
    let map = map_handle.borrow();
    let index = validate_index(&args[1], map.len(), "Iterator")?;
    let mut entries = map.data.iter();
    Ok(entries.nth(index).unwrap().0.clone())
}

fn map_value_iterator_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    let map_handle = unwrap_this_as_map(&args);
    let map = map_handle.borrow();
    let index = validate_index(&args[1], map.len(), "Iterator")?;
    let mut entries = map.data.iter();
    Ok(entries.nth(index).unwrap().1.clone())
}

fn list_filled(vm: &VM, args: &[Value]) -> Result<Value> {
    let size = validate_int(&args[1], "Size")?;
    if size < 0.0 {
        return Err(VMError::from_str("Size cannot be negative."));
    }
    let contents = vec![args[2].clone(); size as usize];
    Ok(Value::List(vm.new_list(contents)))
}

fn list_new(vm: &VM, _args: &[Value]) -> Result<Value> {
    Ok(Value::List(vm.new_list(Vec::new())))
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
        value += len_f;
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

fn list_subscript(vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);

    match &args[1] {
        Value::Num(_) => {
            let index = validate_index(&args[1], list.borrow().len(), "Subscript")?;
            Ok(list.borrow().elements[index].clone())
        }
        Value::Range(r) => {
            let range = calculate_range(&r.borrow(), list.borrow().len())?;
            if range.range.is_empty() {
                return Ok(Value::List(vm.new_list(Vec::new())));
            }
            let slice = &list.borrow().elements[range.range];
            let mut vec = slice.to_vec();
            if range.reverse {
                vec.reverse();
            }
            Ok(Value::List(vm.new_list(vec)))
        }
        _ => Err(VMError::from_str("Subscript must be a number or a range.")),
    }
}

fn list_subscript_setter(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let index = validate_index(&args[1], list.borrow().len(), "Subscript")?;
    list.borrow_mut().elements[index] = args[2].clone();
    Ok(args[2].clone())
}

fn list_add(_vm: &VM, args: &[Value]) -> Result<Value> {
    let value = &args[1];
    let list = unwrap_this_as_list(&args);
    list.borrow_mut().elements.push(value.clone());
    Ok(value.clone())
}

// Adds an element to the list and then returns the list itself. This is called
// by the compiler when compiling list literals instead of using add() to
// minimize stack churn.
fn list_add_core(_vm: &VM, args: &[Value]) -> Result<Value> {
    let value = &args[1];
    let list = unwrap_this_as_list(&args);
    list.borrow_mut().elements.push(value.clone());
    Ok(args[0].clone())
}

fn list_clear(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    list.borrow_mut().elements.clear();
    Ok(Value::Null)
}
fn list_insert(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
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

fn list_remove_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let maybe_index = list.borrow().elements.iter().position(|v| v.eq(&args[1]));
    match maybe_index {
        None => Ok(Value::Null),
        Some(index) => {
            let value = list.borrow_mut().elements.remove(index);
            Ok(value)
        }
    }
}

fn list_index_of(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let maybe_index = list.borrow().elements.iter().position(|v| v.eq(&args[1]));
    Ok(index_or_neg_one(maybe_index))
}

fn list_swap(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let index_a = validate_index(&args[1], list.borrow().len(), "Index 0")?;
    let index_b = validate_index(&args[2], list.borrow().len(), "Index 1")?;

    list.borrow_mut().elements.swap(index_a, index_b);
    Ok(Value::Null)
}

fn validate_int_value(value: f64, arg_name: &str) -> Result<f64> {
    if !f64_is_integer(value) {
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

fn list_iterate(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let elements_len = list.borrow().len() as f64;

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
    Ok(Value::Num(index + 1.0))
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
        value += count as f64;
    }
    // Check bounds.
    if value >= 0.0 && value < count as f64 {
        Ok(value as usize)
    } else {
        Err(VMError::from_string(format!("{} out of bounds.", arg_name)))
    }
}

fn list_iterator_value(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);

    let index = validate_index(&args[1], list.borrow().len(), "Iterator")?;
    let value = list.borrow().elements[index].clone();
    Ok(value)
}

fn list_remove_at(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let index = validate_index(&args[1], list.borrow().len(), "Index")?;
    let value = list.borrow_mut().elements.remove(index);
    Ok(value)
}

fn list_count(_vm: &VM, args: &[Value]) -> Result<Value> {
    let list = unwrap_this_as_list(&args);
    let count = list.borrow().len();
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

fn system_clock(vm: &VM, _args: &[Value]) -> Result<Value> {
    let now = std::time::Instant::now();
    Ok(Value::Num(now.duration_since(vm.start_time).as_secs_f64()))
}

// fn system_gc(_vm: &VM, _args: &[Value]) -> Result<Value> {
//     Ok(Value::Null)
// }

fn system_write_string(vm: &VM, args: &[Value]) -> Result<Value> {
    let string = args[1]
        .try_into_string()
        .ok_or_else(|| VMError::from_str("expected String"))?;
    let result = unescape(&string);
    if let Some(write_fn) = vm.config.write_fn {
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

pub(crate) fn init_base_classes(vm: &mut VM, core_module: &mut Module) {
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
    class.borrow_mut().bind_superclass(&object);
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
    object_metaclass.borrow_mut().bind_superclass(&class);

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
    vm: &mut VM,
    module: &mut Module,
    name: &str,
    superclass: &Handle<ObjClass>,
) -> Handle<ObjClass> {
    let class = vm
        .new_class(&superclass, ClassSource::Internal, name.into())
        .unwrap();
    module
        .define_variable(name, Value::Class(class.clone()))
        .unwrap();
    class
}

// Only used for initing before loading wren_core.wren.
pub(crate) fn init_fn_and_fiber(vm: &mut VM, module: &mut Module) {
    // wren_c compiles wren_core.wren with functions/closures with a
    // null class. Manually initialize classes before compiling wren_core.wren.
    let superclass = module.expect_class("Object");
    vm.fn_class = Some(create_and_define_class(vm, module, "Fn", &superclass));
    // The Fiber used to run wren_core for wren_c has a null class.
    vm.fiber_class = Some(create_and_define_class(vm, module, "Fiber", &superclass));
}

pub(crate) fn register_core_primitives(vm: &mut VM) {
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
    fiber_primitive!(vm, fiber, "transferError(_)", fiber_transfer_error);
    fiber_primitive!(vm, fiber, "try()", fiber_try);
    fiber_primitive!(vm, fiber, "try(_)", fiber_try1);

    let fn_class = vm.fn_class.as_ref().unwrap();
    primitive_static!(vm, fn_class, "new(_)", fn_new);
    primitive!(vm, fn_class, "arity", fn_arity);

    for arity in 0..=crate::vm::MAX_PARAMETERS {
        let name = if arity == 0 {
            "call()".to_string()
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

    primitive_static!(vm, core.num, "fromString(_)", num_from_string);
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

    primitive_static!(vm, core.string, "fromCodePoint(_)", string_from_code_point);
    primitive_static!(vm, core.string, "fromByte(_)", string_from_byte);
    primitive!(vm, core.string, "+(_)", string_plus);
    primitive!(vm, core.string, "[_]", string_subscript);
    primitive!(vm, core.string, "byteAt_(_)", string_byte_at);
    primitive!(vm, core.string, "byteCount_", string_byte_count);
    primitive!(vm, core.string, "codePointAt_(_)", string_code_point_at);
    primitive!(vm, core.string, "contains(_)", string_contains);
    primitive!(vm, core.string, "endsWith(_)", string_ends_with);
    primitive!(vm, core.string, "indexOf(_)", string_index_of1);
    primitive!(vm, core.string, "indexOf(_,_)", string_index_of2);
    primitive!(vm, core.string, "iterate(_)", string_iterate);
    primitive!(vm, core.string, "iterateByte_(_)", string_iterate_byte);
    primitive!(vm, core.string, "iteratorValue(_)", string_iterator_value);
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
    primitive!(vm, map, "iterate(_)", map_iterate);
    primitive!(vm, map, "keyIteratorValue_(_)", map_key_iterator_value);
    primitive!(vm, map, "valueIteratorValue_(_)", map_value_iterator_value);

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
