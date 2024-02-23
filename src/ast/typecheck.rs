use std::error::Error;

use super::nodes;

mod context;

pub fn typecheck(prog: nodes::Program, file_id: usize) -> Result<(), Box<dyn Error>> {
    let mut ctx = context::TypecheckContext::new(file_id);
    ctx.typecheck_program(prog)?;
    Ok(())
}
