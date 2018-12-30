# save ip
/#ip /{h;s///;x;p;d}

# mark instantaneous and register values
s/(addi|muli|bori|bani|gtri|eqri) ([0-9]+) ([0-9]+) ([0-9]+)/\1 \2 \$\3 \4/
s/(seti|gtir|eqir) ([0-9]+) ([0-9]+) ([0-9]+)/\1 \$\2 \3 \4/
s/ ([0-9]+)/ r\1/g

# remove dummy values
s/(seti|setr) ([$r][0-9]+) r[0-9]+ (r[0-9]+)/\1 \2 \3/

# remove ir labels
s/([a-z]+)[ir]/\1/
/bor/!s/([a-z]+)[ir]/\1/

# rename ban and bor to and and or
s/ban/and/
s/bor/or/

# replace ip operations with jmp and rel
G
/(set|add).*r([0-9]+)\n\2/{
    # remove ip register from line
    s/ r[0-9]+\n/\n/
    s/ r([0-9]+)(.*)\n\1/\2\n\1/
    s/set/jmp/
    s/add/rel/
}
s/\n.*$//
