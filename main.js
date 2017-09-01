function instantiate(bytes, imports) {
    return WebAssembly
        .compile(bytes)
        .then(m => new WebAssembly.Instance(m, imports));
}

var DISPLAY_WIDTH = 64;
var DISPLAY_HEIGHT = 32;

var memory;
var HEAP8;
var display = Array(DISPLAY_WIDTH * DISPLAY_HEIGHT);

var importObject = {
    env: {
        random: function() {
            function getRandomInt(min, max) {
                return Math.floor(Math.random() * (max - min)) + min;
            }
            return getRandomInt(0, 256);
        },
        draw: function(x0, y0, i, n) {
            var collision = 0;
            for (var y = 0; y < n; y++) {
                var dy = (y0 + y) % DISPLAY_HEIGHT;
                var spr = HEAP8[i + y];
                for (var x = 0; x < 8; x++) {
                    if ((spr & 0x80) > 0) {
                        var dx = (x0 + x) % DISPLAY_WIDTH;
                        var index = dy * DISPLAY_WIDTH + dx;

                        if (display[index] != 0) {
                            collision = 1;
                        }
                        display[index] ^= 1;
                    }
                    spr <<= 1;
                }
            }
            return collision;
        },
        get_dt: function() {
            return 0;
        },
        set_dt: function(dt) {
        },
        set_st: function(st) {
            console.log("st = " + st);
        },
        wait_key: function() {
            console.log("wait_key()");
            return 0;
        },
        clear_screen: function() {
            console.log("clear_screen");
        },
        is_key_pressed: function(key) {
            console.log("is_key_pressed(key=" + key + ")");
            return 0;
        },
        store_bcd: function(value, i) {
            console.log("store_bcd(value=" + value + ", i=" + i + ")");
        }
    }
};

console.log("instantiating wasm...");

fetch('out.wasm')
    .then(response => response.arrayBuffer())
    .then(bytes => instantiate(bytes, importObject))
    .then(instance => {
        console.log("instance=" + instance);
        memory = instance.exports.mem;
        HEAP8 = new Int8Array(memory.buffer);
        instance.exports.routine_512();
    });