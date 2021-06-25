use crate::vm::*;
use std::ops::*;
use std::rc::Rc;

// System.print is not actually in C in wren_c, but since we can't yet parse
// classes or methods, implementing here to get unit tests working.
pub(crate) fn prim_system_print(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    let value = args.pop().unwrap();
    let string = match &value {
        Value::Null => "null".into(),
        Value::Num(i) => format!("{}", i),
        Value::Boolean(b) => format!("{}", b),
        Value::String(s) => format!("{}", s),
        Value::Class(o) => format!("{:?}", o),
        Value::Range(o) => format!("{:?}", o),
        Value::Fn(_) => format!("Fn {{..}}"),
        // Value::Object(o) => format!("{:?}", o),
    };

    println!("{}", string);
    Ok(value)
}

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

fn range_iterator_value(_vm: &WrenVM, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    // Assuming args[1] is a number.
    Ok(args.pop().unwrap())
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

fn null_not(_vm: &WrenVM, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(true))
}

macro_rules! primitive {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let index = $vm.methods.ensure_method($sig);
        $class
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
    let fn_class =
        wren_new_class(vm, &object, 0, Value::String(Rc::new("Fn".into()))).expect("creating Fn");
    vm.fn_class = Some(fn_class);
}

pub(crate) fn register_core_primitives(vm: &mut WrenVM) {
    let core = CoreClasses {
        bool_class: find_core_class(vm, "Bool"),
        num: find_core_class(vm, "Num"),
        string: find_core_class(vm, "String"),
        null: find_core_class(vm, "Null"),
        range: find_core_class(vm, "Range"),
    };
    primitive!(vm, core.num, "+(_)", num_plus);
    primitive!(vm, core.num, "-(_)", num_minus);
    primitive!(vm, core.num, "-", num_unary_minus);
    primitive!(vm, core.num, "*(_)", num_mult);
    primitive!(vm, core.num, "/(_)", num_divide);
    primitive!(vm, core.num, "<(_)", num_lt);
    primitive!(vm, core.num, "<=(_)", num_lte);
    primitive!(vm, core.num, ">(_)", num_gt);
    primitive!(vm, core.num, ">=(_)", num_gte);
    primitive!(vm, core.num, "..(_)", num_range_inclusive);
    primitive!(vm, core.num, "...(_)", num_range_exclusive);
    primitive!(vm, core.num, "&(_)", num_bitwise_and);
    primitive!(vm, core.num, "|(_)", num_bitwise_or);
    primitive!(vm, core.num, "^(_)", num_bitwise_xor);
    primitive!(vm, core.num, "<<(_)", num_bitwise_shl);
    primitive!(vm, core.num, ">>(_)", num_bitwise_shr);

    primitive!(vm, core.range, "iterate(_)", range_iterate);
    primitive!(vm, core.range, "iteratorValue(_)", range_iterator_value);

    primitive!(vm, core.bool_class, "!", bool_not);

    primitive!(vm, core.null, "!", null_not);

    primitive!(
        vm,
        find_core_class(vm, "System").borrow().class_obj().unwrap(),
        "print(_)",
        prim_system_print
    );
    vm.core = Some(core);
}
