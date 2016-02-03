import os, re, json

os.system('ttf2tfm \'Times New Roman.ttf\' > output.txt')
characters = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

character_width = {}
with open('output.txt') as f:
    for line in f:
        m = re.match(' +[0-9]+ +([0-9a-f]+) +[a-zA-Z]+ +([0-9]+)', line)
        if m and chr(int(m.group(1), 16)) in characters:
            character_width[chr(int(m.group(1), 16))] = int(m.group(2))
with open('character_width.json', 'w') as f:
    f.write(json.dumps(character_width))
