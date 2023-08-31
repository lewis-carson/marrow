use core::mem;
use cranelift_jit_demo::jit;

fn main() -> Result<(), String> {
    // Create the JIT instance, which manages all generated functions and data.
    let mut jit = jit::JIT::default();
    println!("the answer is: {}", run_foo(&mut jit)?);

    Ok(())
}

fn run_foo(jit: &mut jit::JIT) -> Result<isize, String> {
    unsafe { run_code(jit, FOO_CODE, (1, 0)) }
}

/// Executes the given code using the cranelift JIT compiler.
///
/// Feeds the given input into the JIT compiled function and returns the resulting output.
///
/// # Safety
///
/// This function is unsafe since it relies on the caller to provide it with the correct
/// input and output types. Using incorrect types at this point may corrupt the program's state.
unsafe fn run_code<I, O>(jit: &mut jit::JIT, code: &str, input: I) -> Result<O, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(code)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = mem::transmute::<_, fn(I) -> O>(code_ptr);
    // And now we can call it!
    Ok(code_fn(input))
}

// A small test function.
//
// The `(c)` declares a return variable; the function returns whatever value
// it was assigned when the function exits. Note that there are multiple
// assignments, so the input is not in SSA form, but that's ok because
// Cranelift handles all the details of translating into SSA form itself.
// const FOO_CODE: &str = r#"(#f. #x. f x) #a. #b. a"#;
const FOO_CODE: &str = r#"(#x. #y. x) a"#;

/*
S(K K)(S K K)
S((K x)(S((K y)x)))
*/
/*

fn main(x) {
    {
        let a = fn f(f) {
            fn f(x) {
                {
                    let a = {
                        return f;
                    };

                    let b = {
                        return x;
                    };

                    let r = a(b);
                    return r;
                }
            }
        };

        let b = fn f(a) {
            fn f(b) {
                {
                    return a;
                }
            }
        };

        let r = a(b);
        return r;
    }
}
 */
