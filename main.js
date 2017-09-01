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

    render(display) {
        this.clear();
        var i, x, y;
        for (i = 0; i < display.length; i++) {
            x = (i % this.width) * this.cellSize;
            y = Math.floor(i / this.width) * this.cellSize;

            if (display[i] == 1) {
                this.ctx.fillStyle = this.fgColor;
            } else {
                this.ctx.fillStyle = this.bgColor;
            }
            this.ctx.fillRect(x, y, this.cellSize, this.cellSize);
        }
    }
}

var canvas = document.querySelector("canvas");
var renderer = new Renderer(canvas, 64, 32, 8);

var worker = new Worker('worker.js');
worker.onmessage = function (msg) {
    switch (msg.data.type) {
        case "render":
            renderer.render(msg.data.data);
            break;

        case "clear":
            renderer.clear();
            break;
    }
};

worker.postMessage({
    type: "start"
});
