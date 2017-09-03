#!/usr/bin/python

#
# This script is used to generate contents of docs/ folder. This script assumes
# that it is launched from the repository root.
#

import os
import os.path
import subprocess
from string import Template

roms = os.path.join('.', "roms")
docs_dir = os.path.join('.', "docs")

if not os.path.isdir('Chip8-Disassembler') or not os.path.isdir('binaryen'):
    print(
"""Chip8-Disassembler directory is not found.
Try updating submodules with command:
git submodule update --init""")
    exit()

chip8_disassembler_bin = 'Chip8-Disassembler/Chip8-Disassembler'

if not os.path.isfile(chip8_disassembler_bin):
    print(
"""Chip8-Disassembler executable is not found.
Try to build it with:
cd Chip8-Disassembler && make""")
    exit()

wasm_disassembler_bin = 'binaryen/bin/wasm-dis'
if not os.path.isfile(wasm_disassembler_bin):
    print(
"""Binaryen wasm-dis executable is not found.
Try to build it with:
cd binaryen && cmake . && make""")
    exit()

if not os.path.isdir('docs'):
    os.mkdir('docs')

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
    
    rom_disasm_filename = rom_name + '.ch8.txt'
    rom_disasm_path = os.path.join(docs_dir, rom_disasm_filename)
    
    compiled_wasm_filename = rom_name + '.wasm'
    compiled_wasm_path = os.path.join(docs_dir, compiled_wasm_filename)

    wasm_disasm_filename = rom_name + '.wast.txt'
    wasm_disasm_path = os.path.join(docs_dir, wasm_disasm_filename)
    
    # Translate ROM into .wasm binary located at `compiled_wasm_path`.
    subprocess.call("cargo run -- -o %s %s" % (compiled_wasm_path, rom_path), shell=True)
    subprocess.call("%s %s > %s" % (chip8_disassembler_bin, rom_path, rom_disasm_path), shell=True)
    subprocess.call("%s -o %s %s" % (wasm_disassembler_bin, wasm_disasm_path, compiled_wasm_path), shell=True)

    index += '<a href="%s">%s</a><br/>\n' % (player_filename, rom_name)

    player_html = player_template.substitute(
        rom_name=compiled_wasm_filename,
        rom_disasm_filename=rom_disasm_filename,
        wasm_disasm_filename=wasm_disasm_filename)
    with open('docs/' + player_filename, 'w') as player_html_file:
        player_html_file.write(player_html)


index += '''
</body>
</html>
'''

with open('docs/index.html', 'w') as f:
    f.write(index)
