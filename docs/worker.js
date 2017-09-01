function instantiate(bytes, imports) {
    return WebAssembly
        .compile(bytes)
        .then(m => new WebAssembly.Instance(m, imports));
}

var DISPLAY_WIDTH = 64;
var DISPLAY_HEIGHT = 32;

var memory;
var HEAP8;
var SHEAP8;

console.log("instantiating wasm...");


const DISPLAY_MEM_OFFSET = 0;
const DT_MEM_OFFSET = DISPLAY_MEM_OFFSET + (DISPLAY_WIDTH * DISPLAY_HEIGHT);
const ST_MEM_OFFSET = DT_MEM_OFFSET + 1;
const KEYBOARD_MEM_OFFSET = ST_MEM_OFFSET + 1;

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
            var spr = HEAP8[i + y]|0;
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
        return Atomics.load(SHEAP8, DT_MEM_OFFSET);
    }
    set_dt(dt) {
        Atomics.store(SHEAP8, DT_MEM_OFFSET, dt|0 & 0xFF);
    }
    set_st(st) {
        Atomics.store(SHEAP8, ST_MEM_OFFSET, st|0 & 0xFF);
    }
    wait_key() {
        throw "unimplemented";
        return 0;
    }
    clear_screen() {
        var msg = {
            "type": "render"
        };
        for (var x = 0; x < DISPLAY_WIDTH; x++) {
            for (var y = 0; y < DISPLAY_HEIGHT; y++) {
                let index = y * DISPLAY_WIDTH + x + DISPLAY_MEM_OFFSET;
                Atomics.store(SHEAP8, index, 0);
            }
        }
        self.postMessage(msg);
    }
    is_key_pressed(key) {
        let pressed = Atomics.load(SHEAP8, KEYBOARD_MEM_OFFSET + key);
        if (pressed != 0) {
            return 1;
        } else {
            return 0;
        }
    }
    store_bcd(value, i) {
        console.log("store_bcd(value=" + value + ", i=" + i + ")");
        throw "unimplemented";
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
                    instance.exports.routine_512();
                    console.log("routine returned");
            });
            break;
    }
};
