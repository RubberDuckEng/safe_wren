// This is basically a more direct num.abs().log10().floor()
fn nearest_power_of_10(num: f64) -> i32 {
    assert!(num.is_finite());
    let mut exponent = 0;

    let mut tmp = num.abs();

    while tmp < 1.0 && tmp > 0.0 {
        exponent -= 1;
        tmp *= 10.0;
    }
    while tmp >= 10.0 {
        exponent += 1;
        tmp /= 10.0;
    }

    exponent
}

fn power_of_10(exponent: i32) -> f64 {
    10.0f64.powi(exponent)
}

pub fn float_to_shortest_string(num: f64, precision_arg: u16) -> String {
    let mut precision: i32 = precision_arg.into();
    let mut use_e = true;
    let mut exponent = nearest_power_of_10(num);
    let mut integer;
    let mut fraction;

    // This loop is kinda a hack.  We start by assuming we
    // will use e, and then loop back a second time if that's
    // wrong.  We could probably split this into two function
    // calls instead.
    loop {
        let mut abs_num = num.abs();

        if use_e {
            // e uses one of our spots of precision.
            precision -= 1;
            abs_num /= power_of_10(exponent);
        }

        integer = abs_num as u64;
        let mask = power_of_10(precision.into());

        // Convert the fractional part that we care about into
        // an integer for easier handling.
        fraction = (mask * abs_num.fract()).round();
        // If our fraction ended up larger than our mask, round up.
        if fraction >= mask {
            integer += 1;
            fraction = 0.0;
            // We rounded integer up to the next decimal place (10), but
            // we shouldn't show 10.0eX, rather 1.0e(X+1).
            if use_e && integer == 10 {
                integer = 1;
                exponent += 1;
            }
        }

        // Now that we know the final exponent we're to use, we can
        // check to see if use_e false would be shorter. If it would
        // be, just go back and recompute that way.
        if use_e && precision + 1 > exponent && exponent >= -4 {
            precision -= exponent;
            use_e = false;
            continue;
        }
        break;
    }

    let mut buffer = String::new();
    if num.is_sign_negative() {
        buffer.push('-');
    }
    buffer += &integer.to_string();
    if fraction != 0.0 {
        buffer.push('.');
        let frac_str = &fraction.to_string();
        let lead_zeros = precision - (frac_str.len() as i32);
        for _ in 0..lead_zeros {
            buffer.push('0');
        }
        buffer += frac_str.trim_end_matches('0');
    }
    if use_e {
        buffer.push('e');
        if exponent.is_positive() {
            buffer.push('+');
        }
        buffer += &exponent.to_string();
    }
    buffer
}

#[cfg(test)]
mod tests {
    use super::*;

    fn f2s(num: f64) -> String {
        float_to_shortest_string(num, 14)
    }

    #[test]
    fn it_works() {
        assert_eq!(f2s(1.0), "1");
        assert_eq!(f2s(1.1), "1.1");
        assert_eq!(f2s(0.3), "0.3");
        assert_eq!(f2s(0.23456789012345003), "0.23456789012345");
        assert_eq!(f2s(-9007199254740991.0), "-9.007199254741e+15");
        assert_eq!(f2s(1.5707963267948966), "1.5707963267949");
        assert_eq!(f2s(0.9999999999999999), "1");
        assert_eq!(f2s(-0.9999999999999999), "-1");
        assert_eq!(f2s(f64::MAX), "1.7976931348623e+308");
        assert_eq!(f2s(f64::MIN_POSITIVE), "2.2250738585072e-308");
        assert_eq!(f2s(12345678901234.5f64.trunc()), "12345678901234");
    }
}
