use std::fmt::Display;

pub mod nodes;

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Op(Box<Expr>, Opcode, Box<Expr>),
}

#[derive(Debug)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => f.write_fmt(format_args!("{}", n)),
            Self::Op(lhs, opcode, rhs) => f.write_fmt(format_args!("({} {} {})", lhs, opcode, rhs)),
        }?;
        Ok(())
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mul => f.write_str("*"),
            Self::Div => f.write_str("/"),
            Self::Add => f.write_str("+"),
            Self::Sub => f.write_str("-"),
        }?;
        Ok(())
    }
}
