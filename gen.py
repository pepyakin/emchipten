#!/usr/bin/python

import os
import os.path
from string import Template

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


with open("player_template.html") as player_template_file:
    player_template = Template(player_template_file.read())

for rom_name in ROMS:
    rom_path = os.path.join(roms, rom_name)
    player_filename = rom_name + '.html'
    compiled_wasm = rom_name + ".wasm"
    wasm_out = os.path.join(docs_dir, compiled_wasm)
    
    os.system("cargo run -- -o %s %s" % (wasm_out, rom_path))

    index += '<a href="%s">%s</a><br/>\n' % (player_filename, rom_name)

    player_html = player_template.substitute(rom_name=compiled_wasm)
    with open('docs/' + player_filename, 'w') as player_html_file:
        player_html_file.write(player_html)


index += '''
</body>
</html>
'''

with open('docs/index.html', 'w') as f:
    f.write(index)
