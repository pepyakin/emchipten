const DISPLAY_WIDTH = 64;
const DISPLAY_HEIGHT = 32;
const LASTKEY_FUTEX_MEM_OFFSET = 0;
const KEYBOARD_MEM_OFFSET = LASTKEY_FUTEX_MEM_OFFSET + 4;
const DISPLAY_MEM_OFFSET = KEYBOARD_MEM_OFFSET + 16;
const DT_MEM_OFFSET = DISPLAY_MEM_OFFSET + (DISPLAY_WIDTH * DISPLAY_HEIGHT);
const ST_MEM_OFFSET = DT_MEM_OFFSET + 1;

class Renderer {
    constructor(canvas, width, height, cellSize) {
        this.ctx = canvas.getContext("2d");
        this.canvas = canvas;
        this.cellSize = cellSize;
        this.canvas.width = cellSize * DISPLAY_WIDTH;
        this.canvas.height = cellSize * DISPLAY_HEIGHT;
        this.fgColor = "#fff";
        this.bgColor = "#000";
    }

    clear() {
        this.ctx.clearRect(
            0, 
            0, 
            DISPLAY_WIDTH * this.cellSize, 
            DISPLAY_HEIGHT * this.cellSize
        );
    }

    render(videoMem) {
        this.clear();
        var i, x, y;
        for (i = 0; i < (DISPLAY_WIDTH * DISPLAY_HEIGHT); i++) {
            x = (i % DISPLAY_WIDTH) * this.cellSize;
            y = Math.floor(i / DISPLAY_WIDTH) * this.cellSize;

            if (Atomics.load(videoMem, DISPLAY_MEM_OFFSET + i) != 0) {
                this.ctx.fillStyle = this.fgColor;
            } else {
                this.ctx.fillStyle = this.bgColor;
            }
            this.ctx.fillRect(x, y, this.cellSize, this.cellSize);
        }
    }
}

function render(renderer, mem) {
    renderer.render(mem);
    requestAnimationFrame(function() {
        render(renderer, mem);
    });
}

function decrementTimer(mem, timerOffset) {
    while (true) {
        let oldValue = Atomics.load(mem, timerOffset);
        let newValue = oldValue - 1;
        if (newValue < 0) {
            newValue = 0;
        }
        if (oldValue == Atomics.compareExchange(mem, timerOffset, oldValue, newValue)) {
            return newValue;
        }
    }
}

function start(wasmFilename) {    
    var canvas = document.querySelector("canvas");
    let sab = new SharedArrayBuffer(8192);
    let renderer = new Renderer(canvas, 64, 32, 8);

    var worker = new Worker('worker.js');
    worker.onmessage = function (msg) {
        console.log("main received: " + msg.data);
    };

    let SHEAP8 = new Int8Array(sab);
    let SHEAP32 = new Int32Array(sab);

    var keyMapping = {
        "1": 0x1,
        "2": 0x2,
        "3": 0x3,
        "4": 0xC,
        "Q": 0x4,
        "W": 0x5,
        "E": 0x6,
        "R": 0xD,
        "A": 0x7,
        "S": 0x8,
        "D": 0x9,
        "F": 0xE,
        "Z": 0xA,
        "X": 0x0,
        "C": 0xB,
        "V": 0xF
    };

    window.addEventListener("keydown", function(event) {
        var key = keyMapping[String.fromCharCode(event.which)];
        console.log("keydown=" + key);
        if (key !== undefined) {
            if (key < 0x00 || key > 0x0F) {
                throw "key=" + key;
            }
            Atomics.store(SHEAP8, KEYBOARD_MEM_OFFSET + key, 1);
            Atomics.store(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET, key);
            Atomics.wake(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET);
        }
        
    }, false);
    window.addEventListener("keyup", function(event) {
        var key = keyMapping[String.fromCharCode(event.which)];
        console.log("keyup=" + key);
        if (key !== undefined) {
            Atomics.store(SHEAP8, KEYBOARD_MEM_OFFSET + key, 0);
        }
    }, false);

    setInterval(function() {
        let newDt = decrementTimer(SHEAP8, DT_MEM_OFFSET);
        let newSt = decrementTimer(SHEAP8, ST_MEM_OFFSET);

        console.log("newDt=" + newDt + ", newSt=" + newSt);
    }, 1000 / 60);

    Atomics.store(SHEAP32, LASTKEY_FUTEX_MEM_OFFSET, 0xff);
    
    worker.postMessage({
        type: "start",
        wasmFilename,
        sab
    });

    render(renderer, SHEAP8);
}
