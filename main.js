function instantiate(bytes, imports) {
    return WebAssembly
        .compile(bytes)
        .then(m => new WebAssembly.Instance(m, imports));
}

var importObject = {
    env: {
        random: function() {

        },
        draw: function(x, y, n) {
            console.log("draw(x=" + x + ", y=" + y + ", n=" + n + ")");            
        },
        get_dt: function() {

        },
        set_dt: function(dt) {

        },
        set_st: function(st) {
            
        },
        wait_key: function() {
            
        },
        clear_screen: function() {
            console.log("clear_screen");
        },
        is_key_pressed: function(key) {
        },
        store_bcd: function(value, i) {
            
        }
    }
};

fetch('out.wasm')
    .then(response => response.arrayBuffer())
    .then(bytes => instantiate(bytes, importObject))
    .then(instance => {
        console.log("instance=" + instance);
    });