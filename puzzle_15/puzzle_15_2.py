input = open("puzzle_15/input.txt", "r")

map = []
mapFin = False
moves = ""

py = 0
px = 0

rep = lambda x : '##' if x == '#' else '[]' if x == 'O' else '..' if x == '.' else '@.' if x == '@' else '' if x == '\n' else Exception
nub = lambda l : list(dict.fromkeys(l))

targets = []
toMove = []

def insert(y, x, c):
    global map
    map[y] = map[y][:x] + str(c) + map[y][(x+1):]

def movable(dy, dx):
    global targets
    global toMove
    for t in targets:
        y = t[0]
        x = t[1]
        c = map[y][x]
        match c:
            case '.': continue
            case '#': return False
            case '[':
                if not (y, x, '[') in toMove: toMove.append((y, x, '['))
                if not (y, x+1) in targets: targets.append((y, x+1))
                if not (y + dy, x + dx) in targets: targets.append((y + dy, x + dx))
            case ']':
                if not (y, x, ']') in toMove: toMove.append((y, x, ']'))
                if not (y, x-1) in targets: targets.append((y, x-1))
                if not (y + dy, x + dx) in targets: targets.append((y + dy, x + dx))
    return True

def moveBarriers(dy, dx):
    global toMove
    global map
    for m in reversed(nub(toMove)):
        y = m[0]
        x = m[1]
        c = m[2]
        insert(y + dy, x + dx, c)
        insert(y, x, '.')
            
def walk (directions):
    global py
    global px
    global map
    global targets
    global toMove
    for m in directions:
        targets = []
        toMove = []
        match m:
            case '^':
                targets.append((py - 1, px))
                toMove.append((py - 1, px, map[py - 1][px]))
                if map[py - 1][px] == '.':
                    py -= 1
                elif movable(-1, 0):
                    moveBarriers(-1, 0)
                    py -= 1
            case 'v':
                targets.append((py + 1, px))
                toMove.append((py + 1, px, map[py + 1][px]))
                if map[py + 1][px] == '.':
                    py += 1
                elif movable(1, 0):
                    moveBarriers(1, 0)
                    py += 1
            case '>':
                targets.append((py, px + 1))
                toMove.append((py, px + 1, map[py][px + 1]))
                if map[py][px + 1] == '.':
                    px += 1
                elif movable(0, 1):
                    moveBarriers(0, 1)
                    px += 1
            case '<':
                targets.append((py, px - 1))
                toMove.append((py, px - 1, map[py][px - 1]))
                if map[py][px - 1] == '.':
                    px -= 1
                elif movable(0, -1):
                    moveBarriers(0, -1)
                    px -= 1

for x in input:
    if mapFin: moves += x[:-1]
    elif x == "\n": mapFin = True
    else: map.append(''.join(rep(y) for y in x))

try:
    for y in range(len(map)):
        for x in range(len(map[y])):
            if map[y][x] == '@':
                py = y
                px = x
                insert(py, px, '.')
                raise Exception
except Exception:
    None

walk(moves)

sum = 0
for y in range(len(map)):
    for x in range(len(map[y])):
        if map[y][x] == '[':
            sum += 100 * y + x

print(sum)