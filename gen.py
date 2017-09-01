#!/usr/bin/python

import os
import os.path

roms = os.path.join('.', "roms")
docs_dir = os.path.join('.', "docs")

ROMS = [
    "15PUZZLE",
    "BLINKY",
    "BLITZ",
    "BRIX",
    "CONNECT4",
    "GUESS",
    "HIDDEN",
    # "INVADERS", endless loop
    "KALEID",
    "MAZE",
    "MERLIN",
    "MISSILE",
    "PONG",
    "PONG2",
    "PUZZLE",
    "SYZYGY",
    "TANK",
    "TETRIS",
    "TICTAC",
    "UFO",
    "VBRIX",
    "VERS",
    "WIPEOFF",
    # "ZERO", unexpected EOF
]


index = '''
<html>
<head></head>
<body>
'''

for rom_name in ROMS:
    rom_path = os.path.join(roms, rom_name)
    compiled_wasm = rom_name + ".wasm"
    wasm_out = os.path.join(docs_dir, compiled_wasm)
    
    os.system("cargo run -- -o %s %s" % (wasm_out, rom_path))

    index += '<a href="launch.html#%s">%s</a><br/>\n' % (compiled_wasm, rom_name)

index += '''
</body>
</html>
'''

with open('docs/index.html', 'w') as f:
    f.write(index)
