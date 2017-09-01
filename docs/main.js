class Renderer {
    constructor(canvas, width, height, cellSize) {
        this.ctx = canvas.getContext("2d");
        this.canvas = canvas;
        this.width = width;
        this.height = height;
        this.cellSize = cellSize;
        this.canvas.width = cellSize * this.width;
        this.canvas.height = cellSize * this.height;
        this.fgColor = "#fff";
        this.bgColor = "#000";
    }

    clear() {
        this.ctx.clearRect(
            0, 
            0, 
            this.width * this.cellSize, 
            this.height * this.cellSize
        );
    }

    render(videoMem) {
        this.clear();
        var i, x, y;
        for (i = 0; i < (64 * 32); i++) {
            x = (i % this.width) * this.cellSize;
            y = Math.floor(i / this.width) * this.cellSize;

            if (Atomics.load(videoMem, i) == 1) {
                this.ctx.fillStyle = this.fgColor;
            } else {
                this.ctx.fillStyle = this.bgColor;
            }
            this.ctx.fillRect(x, y, this.cellSize, this.cellSize);
        }
    }
}

function render(renderer, videoMem) {
    renderer.render(videoMem);
    requestAnimationFrame(function() {
        render(renderer, videoMem);
    });
}

function start(wasmFilename) {    
    var canvas = document.querySelector("canvas");
    let sab = new SharedArrayBuffer(8192);
    let renderer = new Renderer(canvas, 64, 32, 8);

    var worker = new Worker('worker.js');
    worker.onmessage = function (msg) {
        console.log("main received: " + msg.data);
    };
    
    worker.postMessage({
        type: "start",
        wasmFilename,
        sab
    });

    let videoMem = new Int8Array(sab);
    render(renderer, videoMem);
}
