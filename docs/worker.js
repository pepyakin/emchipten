const DISPLAY_WIDTH = 64;
const DISPLAY_HEIGHT = 32;
const LASTKEY_FUTEX_MEM_OFFSET = 0;
const KEYBOARD_MEM_OFFSET = LASTKEY_FUTEX_MEM_OFFSET + 4;
const DISPLAY_MEM_OFFSET = KEYBOARD_MEM_OFFSET + 16;
const DT_MEM_OFFSET = DISPLAY_MEM_OFFSET + (DISPLAY_WIDTH * DISPLAY_HEIGHT);
const ST_MEM_OFFSET = DT_MEM_OFFSET + 1;

function instantiate(bytes, imports) {
    return WebAssembly
        .compile(bytes)
        .then(m => new WebAssembly.Instance(m, imports));
}

var memory;
var HEAP8;
var SHEAP8, SHEAP32;

console.log("instantiating wasm...");

class Env {
    constructor() {
    }

    random() {
        function getRandomInt(min, max) {
            return Math.floor(Math.random() * (max - min)) + min;
        }
        return getRandomInt(0, 256);
    }
    draw(x0, y0, i, n) {
        var collision = 0;
        for (var y = 0; y < n; y++) {
            var dy = (y0 + y) % DISPLAY_HEIGHT;
            var spr = HEAP8[i + y];
            for (var x = 0; x < 8; x++) {
                if ((spr & 0x80) > 0) {
                    var dx = (x0 + x) % DISPLAY_WIDTH;
                    var index = dy * DISPLAY_WIDTH + dx + DISPLAY_MEM_OFFSET;

                    let oldValue = Atomics.xor(SHEAP8, index, 1);
                    if (oldValue != 0) {
                        collision = 1;
                    }
                }
                spr <<= 1;
            }
        }
        return collision;
    }
    get_dt() {
        let dt = Atomics.load(SHEAP8, DT_MEM_OFFSET)|0 & 0xFF;
        return dt;
    }
    set_dt(dt) {
        console.log("set_dt=" + dt);
        Atomics.store(SHEAP8, DT_MEM_OFFSET, dt|0 & 0xFF);
    }
    set_st(st) {
        console.log("set_st=" + st);
        Atomics.store(SHEAP8, ST_MEM_OFFSET, st|0 & 0xFF);
    }
    wait_key() {
        console.log("wait_key()");
        Atomics.wait(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET, 0xff);
        let lastPressedKey = Atomics.load(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET);
        Atomics.store(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET, 0xff);
        console.log("wait_key()=" + lastPressedKey);
        return lastPressedKey;
    }
    clear_screen() {
        console.log("clear_screen");
        for (var x = 0; x < DISPLAY_WIDTH; x++) {
            for (var y = 0; y < DISPLAY_HEIGHT; y++) {
                let index = y * DISPLAY_WIDTH + x + DISPLAY_MEM_OFFSET;
                Atomics.store(SHEAP8, index, 0);
            }
        }
    }
    is_key_pressed(key) {
        console.log("is_key_pressed(key=" + key + ")");
        let pressed = Atomics.load(SHEAP8, KEYBOARD_MEM_OFFSET + key);
        if (pressed != 0) {
            return 1;
        } else {
            return 0;
        }
    }
    store_bcd(value, i) {
        console.log("store_bcd(value=" + value + ", i=" + i + ")");
        HEAP8[i]     = (value / 100)|0 & 0xFF;
        HEAP8[i + 1] = (value % 100 / 10)|0 & 0xFF;
        HEAP8[i + 2] = (value % 10)|0 & 0xFF;
    }
}

onmessage = function(e) {
    console.log("worker message + " + e.data);
    switch (e.data.type) {
        case "start":
            let wasmFilename = e.data.wasmFilename;
            let sab = e.data.sab;
            
            console.log("starting " + wasmFilename);

            SHEAP8 = new Int8Array(sab);
            SHEAP32 = new Int32Array(sab);

            let env = new Env();
            let imports = {
                env
            };

            fetch(wasmFilename)
                .then(response => response.arrayBuffer())
                .then(bytes => instantiate(bytes, imports))
                .then(instance => {
                    console.log("instance=" + instance);
                    memory = instance.exports.mem;
                    HEAP8 = new Int8Array(memory.buffer);
                    try {
                        instance.exports.routine_512();
                    } catch (err) {
                        console.log("err=" + err);
                    } finally {
                        console.log("start returned!");
                    }
            });
            break;
    }
};
