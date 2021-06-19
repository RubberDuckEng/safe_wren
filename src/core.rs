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
        Value::Class(o) => format!("{:?}", o),
        Value::Range(o) => format!("{:?}", o),
        // Value::Object(o) => format!("{:?}", o),
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

macro_rules! primitive {
    ($vm:expr, $class:expr, $sig:expr, $func:expr) => {
        let index = $vm.methods.ensure_method($sig);
        $class
            .borrow_mut()
            .set_method(index, Method::Primitive($func));
    };
}

pub(crate) fn init_core_classes(vm: &mut WrenVM) {
    // wren_c makes a core module, which it then imports
    // into every module when running.  For now we're just
    // "importing" core directly into the one module we ever have.

    // FIXME: Store core_module in module map.
    // Define the root Object class. This has to be done a little specially
    // because it has no superclass.
    let object = define_class(&mut vm.module, "Object");

    // PRIMITIVE(vm->objectClass, "!", object_not);
    // PRIMITIVE(vm->objectClass, "==(_)", object_eqeq);
    // PRIMITIVE(vm->objectClass, "!=(_)", object_bangeq);
    // PRIMITIVE(vm->objectClass, "is(_)", object_is);
    // PRIMITIVE(vm->objectClass, "toString", object_toString);
    // PRIMITIVE(vm->objectClass, "type", object_type);
    // Now we can define Class, which is a subclass of Object.
    let class = define_class(&mut vm.module, "Class");
    class.borrow_mut().superclass = Some(object.clone());
    primitive!(vm, class, "name", class_name);

    // PRIMITIVE(vm->classClass, "name", class_name);
    // PRIMITIVE(vm->classClass, "supertype", class_supertype);
    // PRIMITIVE(vm->classClass, "toString", class_toString);
    // PRIMITIVE(vm->classClass, "attributes", class_attributes);
    // Finally, we can define Object's metaclass which is a subclass of Class.
    let object_metaclass = define_class(&mut vm.module, "Object metaclass");
    // Wire up the metaclass relationships now that all three classes are built.
    object.borrow_mut().class = Some(object_metaclass.clone());
    object_metaclass.borrow_mut().class = Some(class.clone());
    class.borrow_mut().class = Some(class.clone());
    object_metaclass.borrow_mut().superclass = Some(class.clone());
    // PRIMITIVE(objectMetaclass, "same(_,_)", object_same);

    vm.core = Some(CoreClasses {
        bool_class: base_class(&mut vm.module, "Bool", &object, &class),
        num: base_class(&mut vm.module, "Num", &object, &class),
        string: base_class(&mut vm.module, "String", &object, &class),
        system: base_class(&mut vm.module, "System", &object, &class),
        null: base_class(&mut vm.module, "Null", &object, &class),
        range: base_class(&mut vm.module, "Range", &object, &class),
        class: class,
    });
}

pub(crate) fn register_core_primitives(vm: &mut WrenVM) {
    let core = vm.core.as_ref().unwrap();
    primitive!(vm, core.num, "+(_)", num_plus);
    primitive!(vm, core.num, "-(_)", num_minus);
    primitive!(vm, core.num, "-", num_unary_minus);
    primitive!(vm, core.num, "*(_)", num_mult);
    primitive!(vm, core.num, "/(_)", num_divide);
    primitive!(vm, core.num, "<(_)", num_lt);
    primitive!(vm, core.num, ">(_)", num_gt);
    primitive!(vm, core.num, "..(_)", num_range_inclusive);
    primitive!(vm, core.num, "...(_)", num_range_exclusive);
}
