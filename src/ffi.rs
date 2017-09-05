use std::os::raw::c_uchar;
use std::slice;
use super::build_rom;

#[no_mangle]
pub extern fn build_rom_raw(rom_ptr: *const c_uchar, rom_size: usize, out_ptr: *mut c_uchar, out_size: usize) -> bool {
    let rom = unsafe {
        slice::from_raw_parts(rom_ptr, rom_size)
    };

    let result_wasm = match build_rom(rom, Default::default()) {
        Ok(wasm) => wasm,
        Err(e) => {
            println!("{}", e);
            return false;
        }
    };
    if result_wasm.len() == 0 {
        println!("resulting wasm is empty");
        return false;
    }
    if result_wasm.len() > out_size {
        println!("resulting wasm is too big: {}", result_wasm.len());
        return false;
    }

    let out = unsafe {
        slice::from_raw_parts_mut(out_ptr, result_wasm.len())
    };
    out.copy_from_slice(&result_wasm);

    true
}
