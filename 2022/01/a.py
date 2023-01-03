import sys

elves = [[]]
for line in sys.stdin:
    match line.strip():
        case "": elves.append([])
        case line: elves[-1].append(int(line))
print(max(sum(elf_items) for elf_items in elves))
