use std::error::Error;

use super::nodes;

mod context;

pub fn typecheck(prog: nodes::Program) -> Result<(), Box<dyn Error>> {
    let mut ctx = context::TypecheckContext::new();
    ctx.typecheck_program(prog)?;
    Ok(())
}
