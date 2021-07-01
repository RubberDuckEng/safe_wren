use crate::vm::*;
use std::collections::VecDeque;
use std::ops::*;

macro_rules! num_constant {
    ($func:ident, $value:expr) => {
        fn $func(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
            Ok(Value::Num($value))
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

macro_rules! infix_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
            Ok(Value::$return_type(
                args[0].try_into_num()?.$method(&args[1].try_into_num()?),
            ))
        }
    };
}

infix_num_op!(num_plus, add, Num);
infix_num_op!(num_minus, sub, Num);
infix_num_op!(num_mult, mul, Num);
infix_num_op!(num_divide, div, Num);
infix_num_op!(num_lt, lt, Boolean);
infix_num_op!(num_gt, gt, Boolean);
infix_num_op!(num_lte, le, Boolean);
infix_num_op!(num_gte, ge, Boolean);

macro_rules! bitwise_num_op {
    ($func:ident, $method:ident, $return_type:ident) => {
        fn $func(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let a = args[0].try_into_num()? as u32;
            let b = args[1].try_into_num()? as u32;
            Ok(Value::$return_type(a.$method(&b) as f64))
        }
    };
}

bitwise_num_op!(num_bitwise_and, bitand, Num);
bitwise_num_op!(num_bitwise_or, bitor, Num);
bitwise_num_op!(num_bitwise_xor, bitxor, Num);
bitwise_num_op!(num_bitwise_shl, shl, Num);
bitwise_num_op!(num_bitwise_shr, shr, Num);

fn num_unary_minus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Num(-args[0].try_into_num()?))
}
fn num_range_inclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let start = args[0].try_into_num()?;
    let end = args[1].try_into_num()?;
    Ok(Value::Range(wren_new_range(vm, start, end, true)))
}
fn num_range_exclusive(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let start = args[0].try_into_num()?;
    let end = args[1].try_into_num()?;
    Ok(Value::Range(wren_new_range(vm, start, end, false)))
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
    if num == -0.0 && num.is_sign_negative() {
        return "-0".into();
    }

    // wren_c uses sprintf(buffer, "%.14g", value)
    // It's not clear how to set a maximum precision without also
    // forcing rust to use that precision always.
    // e.g. "{:.14}" would always print 14 digits.
    format!("{}", num)
}

fn num_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let string = wren_num_to_string(args[0].try_into_num()?);
    Ok(Value::from_string(string))
}

fn class_name(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let this = args[0].try_into_class()?;
    let string = this.borrow().name.clone();
    Ok(Value::from_string(string))
}

fn object_eqeq(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(args[0].eq(&args[1])))
}

fn object_bangeq(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(args[0].ne(&args[1])))
}

fn object_is(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let expected_baseclass = args[1].try_into_class()?;
    let mut class = vm
        .class_for_value(&args[0])
        .ok_or(RuntimeError::ObjectRequired(args[0].clone()))?;
    // Should this just be an iterator?
    // e.g. for class in object.class_chain()

    loop {
        if *expected_baseclass.borrow() == *class.borrow() {
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

fn range_iterate(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let range_cell = args[0].try_into_range()?;
    let range = range_cell.borrow();

    // Special case: empty range.
    if range.from == range.to && !range.is_inclusive {
        return Ok(Value::Boolean(false)); // No more elements.
    }
    // Start the iteration.
    if args[1].is_null() {
        return Ok(Value::Num(range.from));
    }

    let mut iterator = args[1].try_into_num()?;

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

fn range_iterator_value(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    // Assuming args[1] is a number.
    Ok(args[1].clone())
}

fn range_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let range_ref = args[0].try_into_range()?;
    let range = range_ref.borrow();

    let from = wren_num_to_string(range.from);
    let to = wren_num_to_string(range.to);
    let op = if range.is_inclusive { ".." } else { "..." };
    Ok(Value::from_string(format!("{}{}{}", from, op, to)))
}

fn object_type(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let class = vm
        .class_for_value(&args[0])
        .ok_or(RuntimeError::ObjectRequired(args[0].clone()))?;
    Ok(Value::Class(class))
}

fn object_not(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(false))
}

fn bool_not(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(!args[0].equals_true()))
}

fn bool_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args[0].equals_true() {
        Ok(Value::from_str("true"))
    } else {
        Ok(Value::from_str("false"))
    }
}

fn fiber_abort(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let is_abort = !args[1].is_null();
    // wren_c just records the error string and continues?
    // vm->fiber->error = args[1];
    if is_abort {
        Err(RuntimeError::FiberAbort(args[1].clone()))
    } else {
        // I guess Fiber.abort(null) clears the error?
        // wren_c: If the error is explicitly null, it's not really an abort.
        Ok(Value::Boolean(args[1].is_null()))
    }
}

fn null_not(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(true))
}

fn null_to_string(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::from_str("null"))
}

fn string_plus(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let a = args[0].try_into_string()?;
    let b = args[1].try_into_string()?;

    Ok(Value::from_string(a + &b))
}

fn string_to_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    // Do we need to confirm args[0] is a string?  wren_c does not.
    Ok(args[0].clone())
}

fn fn_new(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let closure = args[1].try_into_closure()?;
    Ok(Value::Closure(closure))
}

fn fn_arity(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let closure = args[0].try_into_closure()?;
    let arity = closure.borrow().fn_obj.borrow().arity as f64;
    Ok(Value::Num(arity))
}

fn fn_to_string(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::from_str("<fn>"))
}

fn list_new(vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::List(wren_new_list(vm)))
}

// Adds an element to the list and then returns the list itself. This is called
// by the compiler when compiling list literals instead of using add() to
// minimize stack churn.
fn list_add_core(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    let value = args.pop().unwrap();
    let list_value = args.pop().unwrap();
    let list = list_value.try_into_list()?;
    list.borrow_mut().elements.push(value);
    Ok(list_value)
}

fn list_count(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let list = args[0].try_into_list()?;
    let count = list.borrow().elements.len() as f64;
    Ok(Value::Num(count))
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

fn system_write_string(_vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let string = args[1].try_into_string()?;
    let result = unescape(&string);
    // FIXME: This should be an API call to the embedder.
    print!("{}", result);
    Ok(args[1].clone())
}

macro_rules! primitive {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let index = $vm.methods.ensure_method($sig);
        $class
            .borrow_mut()
            .set_method(index, Method::Primitive($func));
    };
}

macro_rules! primitive_static {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let index = $vm.methods.ensure_method($sig);
        $class
            .borrow_mut()
            .class_obj()
            .unwrap()
            .borrow_mut()
            .set_method(index, Method::Primitive($func));
    };
}

pub(crate) fn init_base_classes(vm: &mut WrenVM) {
    // wren_c makes a core module, which it then imports
    // into every module when running.  For now we're just
    // "importing" core directly into the one module we ever have.

    // FIXME: Store core_module in module map.
    // Define the root Object class. This has to be done a little specially
    // because it has no superclass.
    let object = define_class(&mut vm.module, "Object");
    primitive!(vm, object, "!", object_not);
    primitive!(vm, object, "==(_)", object_eqeq);
    primitive!(vm, object, "!=(_)", object_bangeq);
    primitive!(vm, object, "is(_)", object_is);
    // PRIMITIVE(vm->objectClass, "toString", object_toString);
    primitive!(vm, object, "type(_)", object_type);

    // Now we can define Class, which is a subclass of Object.
    let class = define_class(&mut vm.module, "Class");
    wren_bind_superclass(&mut class.borrow_mut(), &object);
    primitive!(vm, class, "name", class_name);
    // PRIMITIVE(vm->classClass, "supertype", class_supertype);
    // PRIMITIVE(vm->classClass, "toString", class_toString);
    // PRIMITIVE(vm->classClass, "attributes", class_attributes);

    // Finally, we can define Object's metaclass which is a subclass of Class.
    let object_metaclass = define_class(&mut vm.module, "Object metaclass");
    // Wire up the metaclass relationships now that all three classes are built.
    object.borrow_mut().class = Some(object_metaclass.clone());
    object_metaclass.borrow_mut().class = Some(class.clone());
    class.borrow_mut().class = Some(class.clone());
    wren_bind_superclass(&mut object_metaclass.borrow_mut(), &class);
    // PRIMITIVE(objectMetaclass, "same(_,_)", object_same);
    vm.class_class = Some(class);

    // Hack.  AFAICT, functions/closures inside wren_core.wren are compiled
    // by wren_c and have a null class!  Rust won't allow us to do that so
    // manually initialize Fn class before compiling wren_core.wren.
    let fn_class = wren_new_class(vm, &object, 0, "Fn".into()).expect("creating Fn");
    vm.fn_class = Some(fn_class.clone());
    wren_define_variable(&mut vm.module, "Fn", Value::Class(fn_class)).expect("defining Fn");
}

pub(crate) fn register_core_primitives(vm: &mut WrenVM) {
    // Note: All of these methods are bound *after* setup from
    // superclass, so any classes added to a superclass
    // WOULD NOT end up inherited into wren_core.wren subclasses.

    let core = CoreClasses {
        bool_class: find_core_class(vm, "Bool"),
        num: find_core_class(vm, "Num"),
        string: find_core_class(vm, "String"),
        null: find_core_class(vm, "Null"),
        range: find_core_class(vm, "Range"),
        list: find_core_class(vm, "List"),
    };

    primitive!(vm, core.bool_class, "!", bool_not);
    primitive!(vm, core.bool_class, "toString", bool_to_string);

    let fiber = find_core_class(vm, "Fiber");
    primitive_static!(vm, fiber, "abort(_)", fiber_abort);

    // primitive!(vm, core.num, "fromString(_)", num_from_string);
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
    primitive!(vm, core.num, "-", num_unary_minus);
    primitive!(vm, core.num, "..(_)", num_range_inclusive);
    primitive!(vm, core.num, "...(_)", num_range_exclusive);
    primitive!(vm, core.num, "toString", num_to_string);

    primitive!(vm, core.string, "+(_)", string_plus);
    primitive!(vm, core.string, "toString", string_to_string);

    let list = find_core_class(vm, "List");
    // primitive_static!(vm, list, "filled(_,_)", list_filled);
    primitive_static!(vm, list, "new()", list_new);
    primitive!(vm, list, "addCore_(_)", list_add_core);
    primitive!(vm, list, "count", list_count);

    primitive!(vm, core.range, "iterate(_)", range_iterate);
    primitive!(vm, core.range, "iteratorValue(_)", range_iterator_value);
    primitive!(vm, core.range, "toString", range_to_string);

    primitive!(vm, core.null, "!", null_not);
    primitive!(vm, core.null, "toString", null_to_string);

    let fn_class = vm.fn_class.as_ref().unwrap();
    primitive_static!(vm, fn_class, "new(_)", fn_new);
    primitive!(vm, fn_class, "arity", fn_arity);
    primitive!(vm, fn_class, "toString", fn_to_string);

    for arity in 0..16 {
        let name = if arity == 0 {
            format!("call()")
        } else {
            // arity=1 -> "call(_)", arity=2 -> "call(_,_)", etc.
            format!("call({}{})", "_,".repeat(arity - 1), "_")
        };
        let symbol = vm.methods.ensure_method(&name);
        fn_class
            .borrow_mut()
            .set_method(symbol, Method::FunctionCall);
    }

    let system = find_core_class(vm, "System");
    primitive_static!(vm, system, "writeString_(_)", system_write_string);

    vm.core = Some(core);

    // wren_c walks *all* String objects here to bind a class pointer
    // to them.  We don't currently need to do that, but may need
    // to do that eventually for other types.
}
