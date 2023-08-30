use std::{collections::HashMap, fmt::Display};

use either::Either;

/// The AST node for expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Variable(String),
    Abstraction(Vec<Box<Expr>>, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Variable(n) => write!(f, "{}", n),
            Expr::Abstraction(x, s) => write!(f, "(λ{}. {})", x[0], s),
            Expr::Application(a, b) => write!(f, "({} {})", a, b),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    args: Vec<String>,
    body: Either<String, Call>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*
        fn name(...args){
            body
        }
         */

        write!(f, "fn {}(", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }

        write!(f, ") {{\n    ")?;

        match &self.body {
            Either::Left(e) => write!(f, "{}", e)?,
            Either::Right(c) => write!(f, "{}", c)?,
        }

        write!(f, "\n}}")
    }
}

#[derive(Debug)]
pub struct Call {
    name: String,
    args: Vec<String>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*
        name(...args)
         */

        write!(f, "{}(", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }

        write!(f, ")")
    }
}

impl Expr {
    pub fn make_unique_names(&mut self) -> Self {
        // alpha renaming
        let mut sm = HashMap::<String, String>::new();

        let mut c = 0;

        fn go(env: &mut HashMap<String, String>, e: &mut Expr, c: &mut usize) -> Expr {
            let transformed_expr = match e {
                Expr::Variable(x) => Expr::Variable(env.get(x).unwrap_or(x).clone()),

                Expr::Abstraction(x, s) => {
                    let name = match x[0].as_ref() {
                        Expr::Variable(x) => x,
                        _ => panic!("expected variable"),
                    };

                    let x_prime = {
                        *c += 1;
                        format!("{}{}", x[0], c)
                    };

                    env.insert(name.clone(), x_prime.clone());

                    Expr::Abstraction(
                        vec![Box::new(Expr::Variable(x_prime))],
                        Box::new(go(env, s, c)),
                    )
                }
                Expr::Application(a, b) => {
                    Expr::Application(Box::new(go(env, a, c)), Box::new(go(env, b, c)))
                }
            };

            transformed_expr
        }

        go(&mut sm, self, &mut c)
    }
}

#[derive(Debug, Clone)]
pub enum C {
    Function(String, String, Box<C>),
    Block(Vec<Box<C>>),
    Let(String, Box<C>),
    Call(String, String),
    Return(Box<C>),
    Variable(String),
}

impl C {
    pub fn from(expr: Expr) -> Self {
        fn go(e: Expr, c: &mut usize) -> C {
            match e {
                Expr::Variable(x) => {
                    /*
                    return x
                     */
                    *c += 1;
                    C::Let(format!("f{}", *c), Box::new(C::Variable(x)))
                }
                Expr::Application(a, b) => {
                    /*
                    let r = f(a)
                    return r

                     */

                    let left = go(*a, c);
                    let right = go(*b, c);

                    let left_name = match left.clone() {
                        C::Function(name, _, _) => name,
                        C::Variable(name) => name,
                        C::Let(name, _) => name,
                        C::Return(a) => match *a {
                            C::Function(name, _, _) => name,
                            _ => panic!("expected function inside return"),
                        },
                        x => panic!("expected let, got {:?}", x),
                    };

                    let right_name = match right.clone() {
                        C::Function(name, _, _) => name,
                        C::Variable(name) => name,
                        C::Let(name, _) => name,
                        C::Return(a) => match *a {
                            C::Function(name, _, _) => name,
                            _ => panic!("expected function inside return"),
                        },
                        x => panic!("expected let, got {:?}", x),
                    };

                    C::Block(vec![
                        Box::new(left),
                        Box::new(right),
                        Box::new(C::Let(
                            "r".to_owned(),
                            Box::new(C::Call(left_name, right_name)),
                        )),
                        Box::new(C::Return(Box::new(C::Variable("r".to_owned())))),
                    ])
                }
                Expr::Abstraction(x, s) => {
                    /*
                    fn f(x) {
                        go(s)
                    }
                     */

                    let name = match x[0].as_ref() {
                        Expr::Variable(x) => x,
                        _ => panic!("expected variable"),
                    };

                    *c += 1;

                    C::Function(format!("f{c}"), name.clone(), Box::new(go(*s, c)))
                }
            }
        }

        C::Function(
            "main".to_owned(),
            "x".to_owned(),
            Box::new(go(expr, &mut 0)),
        )
    }

    fn add_orphan_returns(&mut self) -> Self {
        fn go(c: C) -> C {
            match c {
                C::Function(name, arg, body) => {
                    /* */
                    let body = match *body {
                        C::Block(body) => body,
                        _ => panic!("expected block"),
                    };

                    println!("function: {}", name);

                    println!("body: {:?}", body);
                    println!("body: {:?}", body.len());

                    let body = if body.len() == 1 {
                        body[0].clone()
                    } else {
                        Box::new(C::Block(body))
                    };

                    C::Function(name, arg, body)
                }
                C::Block(body) => {
                    let mut new_body = Vec::new();

                    for c in body {
                        new_body.push(Box::new(go(*c)));
                    }

                    C::Block(new_body)
                }
                c => c,
            }
        }

        go(self.clone())
    }

    pub fn to_string(&self) -> String {
        fn go(c: &C, indent: usize) -> String {
            let mut s = String::new();

            for _ in 0..indent {
                s.push_str("    ");
            }

            match c {
                C::Function(name, arg, body) => {
                    s.push_str(&format!("fn {}({})\n", name, arg));

                    s.push_str(&go(body, indent + 1));
                    s.push_str("\n");

                    for _ in 0..indent {
                        s.push_str("    ");
                    }
                }
                C::Block(body) => {
                    s.push_str("{\n");

                    for c in body {
                        s.push_str(&go(c, indent + 1));
                        s.push_str("\n");
                    }

                    for _ in 0..indent {
                        s.push_str("    ");
                    }

                    s.push_str("}");
                }
                C::Let(name, value) => {
                    s.push_str(&format!("let {} = {};", name, go(value, 0)));
                }
                C::Call(name, arg) => {
                    s.push_str(&format!("{}({})", name, arg));
                }
                C::Return(name) => {
                    s.push_str(&format!("return {};", go(name, 0)));
                }
                C::Variable(name) => {
                    s.push_str(name);
                }
            }

            s
        }

        go(self, 0)
    }
}

peg::parser!(pub grammar parser() for str {
    pub rule expression() -> Expr
        = precedence! {
            e:variable() { e }
            --
            "#" _ x:variable() _ "." _ s:@ {
                Expr::Abstraction(vec![Box::new(x)], Box::new(s))
            }
            --
            a:(@) _ b:@ {
                Expr::Application(Box::new(a), Box::new(b))
            }
            --
            "(" _ e:expression() _ ")" { e }
            --
            "(" _ "#" _ x:variable() _ "." _ s:expression() _ ")" {
                Expr::Abstraction(vec![Box::new(x)], Box::new(s))
            }
        }


    rule variable() -> Expr
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Expr::Variable(n.to_owned()) } }
        / expected!("identifier")

    rule _() =  quiet!{[' ' | '\t']*}
});

/*
fn main(x) {
    {
        return fn f1(f) {
            return fn f2(x) {
                {
                    let f3 = f;
                    let f4 = x;
                    let r = f3(f4);
                    return r;
                }
            };
        };

        return fn f5(a) {
            return fn f6(b) {
                let f7 = a;
            };
        };

        let r = f1(f5);
        return r;
    }
}
*/
