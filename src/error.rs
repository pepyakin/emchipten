use instruction;
use std::io;

error_chain! {
    errors {
        UnrecognizedInstruction(iw: instruction::InstructionWord)
    }

    foreign_links {
        Io(io::Error);
    }
}
