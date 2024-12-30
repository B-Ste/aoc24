input = open("puzzle_19/input.txt", "r").readlines()
towels = input[0].split(', ')
towels[len(towels) - 1] = towels[len(towels) - 1].strip()
patterns = map(lambda x: x.strip(), input[2:])
mem = {}

def solve(s : str):
    if s in mem: 
        return mem[s]
    else:
        if s == "": return 1
        prefixes = filter(lambda t: s.startswith(t), towels)
        if not prefixes: return 0
        sum = 0
        for p in prefixes:
            sum += solve(s.removeprefix(p))
        mem.update({s: sum})
        return sum

sum = 0
for p in patterns:
    sum += solve(p)

print(sum)
