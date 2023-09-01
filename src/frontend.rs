use std::{collections::HashMap, fmt::Display};

use either::Either;

/// The AST node for expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Variable(String),
    Abstraction(Vec<String>, Box<Expr>),
    Application(Box<Expr>, Vec<Box<Expr>>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Variable(n) => write!(f, "{}", n),
            Expr::Abstraction(x, s) => write!(f, "(λ{}. {})", x.join(", "), s),
            Expr::Application(a, b) => {
                let joined_b = b
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "({} ({}))", a, joined_b)
            }
        }
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
                    let arg = x[0].clone();

                    let x_prime = {
                        *c += 1;
                        format!("{}{}", arg, c)
                    };

                    env.insert(arg, x_prime.clone());

                    Expr::Abstraction(vec![x_prime], Box::new(go(env, s, c)))
                }
                Expr::Application(a, b) => Expr::Application(
                    Box::new(go(env, a, c)),
                    vec![Box::new(go(env, &mut b[0], c))],
                ),
            };

            transformed_expr
        }

        go(&mut sm, self, &mut c)
    }

    pub fn convert_closures(&self) -> Self {
        fn free_variables(e: &Expr, bound: Vec<String>) -> Vec<String> {
            match e {
                Expr::Variable(x) => {
                    let mut fv = vec![];
                    fv.extend(bound.clone());

                    if !bound.contains(x) {
                        fv.push(x.clone());
                    }

                    fv
                }
                Expr::Abstraction(x, s) => {
                    let mut bound = bound.clone();
                    bound.extend(x.clone());

                    free_variables(s, bound)
                }
                Expr::Application(a, b) => {
                    let mut fv = free_variables(a, bound.clone());
                    fv.extend(free_variables(&b[0], bound));

                    fv
                }
            }
        }

        fn go(e: Expr) -> Expr {
            match e {
                Expr::Variable(x) => Expr::Variable(x.to_string()),
                Expr::Abstraction(x, s) => {
                    let mut x_prime = free_variables(&s, x.clone());

                    x_prime.sort();
                    x_prime.dedup();

                    Expr::Abstraction(x_prime, Box::new(go(*s)))
                }
                Expr::Application(a, b) => {
                    Expr::Application(Box::new(go(*a)), vec![Box::new(go(*b[0].clone()))])
                }
            }
        }

        fn collapse(e: Expr) -> Expr {
            // (λf1, x2. (λf1, x2. (f1 x2))) -> (λf1, x2. (f1 x2))
            match e {
                Expr::Variable(x) => Expr::Variable(x),
                Expr::Abstraction(a, b) => match *b.clone() {
                    Expr::Abstraction(c, d) => {
                        if c == a {
                            Expr::Abstraction(c, Box::new(collapse(*d)))
                        } else {
                            Expr::Abstraction(a, Box::new(collapse(*b)))
                        }
                    }
                    _ => Expr::Abstraction(a, Box::new(collapse(*b))),
                },
                Expr::Application(a, b) => {
                    match *a.clone() {
                        Expr::Application(c, d) => {
                            // inline d, then b
                            let inline = vec![
                                Box::new(collapse(*d[0].clone())),
                                Box::new(collapse(*b[0].clone())),
                            ];

                            Expr::Application(Box::new(collapse(*c)), inline)
                        }
                        _ => Expr::Application(
                            Box::new(collapse(*a)),
                            vec![Box::new(collapse(*b[0].clone()))],
                        ),
                    }
                }
            }
        }

        go(self.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HoistedExpr(HashMap<String, Expr>);

/*
to hoist,

C = (x)
D = (x)
B = (λx. D)
A = (B C)
MAIN = (λx. A)

(λx (λx. x) (x))

*/

impl HoistedExpr {
    pub fn from(expr: Expr) -> HoistedExpr {
        let mut lets = HashMap::new();

        fn redirect_application(
            map: &mut HashMap<String, Expr>,
            f: String,
            a: String,
            b: Vec<String>,
        ) {
            match map.get(&f).unwrap() {
                Expr::Variable(_) => panic!(""),
                Expr::Application(_, _) => {
                    map.get_mut(&f).map(|s| {
                        *s = Expr::Application(
                            Box::new(Expr::Variable(a)),
                            b.iter()
                                .map(|x| Box::new(Expr::Variable(x.clone())))
                                .collect(),
                        )
                    });
                }
                Expr::Abstraction(_, _) => panic!(""),
            };
        }

        fn redirect_abstraction(map: &mut HashMap<String, Expr>, f: String, s: String) {
            match map.get(&f).unwrap() {
                Expr::Variable(_) => panic!(""),
                Expr::Application(_, _) => panic!(""),
                Expr::Abstraction(x, _) => {
                    let z = x.clone();
                    map.get_mut(&f)
                        .map(|m| *m = Expr::Abstraction(z, Box::new(Expr::Variable(s))));
                }
            };
        }

        fn go(expr_name: String, expr: Expr, lets: &mut HashMap<String, Expr>, c: &mut usize) {
            match expr {
                Expr::Variable(x) => {}
                Expr::Abstraction(x, s) => {
                    *c += 1;

                    let s_name = format!("F{}", *c);

                    lets.insert(s_name.clone(), *s.clone());

                    // rename with redirect
                    redirect_abstraction(lets, expr_name, s_name.clone());

                    go(s_name, *s, lets, c);
                }
                Expr::Application(a, b) => {
                    *c += 1;

                    let a_name = format!("F{}", *c);

                    let mut redirects = vec![];

                    for stmt in b {
                        *c += 1;
                        let stmt_name = format!("F{}", *c);
                        lets.insert(stmt_name.clone(), *stmt.clone());
                        go(stmt_name.clone(), *stmt, lets, c);
                        redirects.push(stmt_name);
                    }

                    lets.insert(a_name.clone(), *a.clone());

                    // rename with redirect
                    redirect_application(lets, expr_name, a_name.clone(), redirects);

                    go(a_name, *a, lets, c);
                }
            }
        }

        lets.insert("F0".to_string(), expr.clone());
        go("F0".to_string(), expr, &mut lets, &mut 0);

        HoistedExpr(lets)
    }
}

impl Display for HoistedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        // turn map into list of (String, Expr) and sort by String
        let mut lets: Vec<(&String, &Expr)> = self.0.iter().collect();

        lets.sort_by(|a, b| b.0.cmp(a.0));

        for (name, expr) in lets {
            s.push_str(&format!("let {} = {};\n", name, expr));
        }

        write!(f, "{}", s)
    }
}

peg::parser!(pub grammar parser() for str {
    pub rule expression() -> Expr
        = precedence! {
            e:variable() { e }
            --
            "#" _ x:variable() _ "." _ s:@ {
                Expr::Abstraction(vec![x.to_string()], Box::new(s))
            }
            --
            a:(@) _ b:@ {
                Expr::Application(Box::new(a), vec![Box::new(b)])
            }
            --
            "(" _ e:expression() _ ")" { e }
            --
            "(" _ "#" _ x:variable() _ "." _ s:expression() _ ")" {
                Expr::Abstraction(vec![x.to_string()], Box::new(s))
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
