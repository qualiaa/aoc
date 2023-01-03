from sys import stdin
from typing import Callable, Iterator


class Directory:
    def __init__(self, parent=None):
        """An empty directory ready to be updated with contents.

        A directory is three things:

          1. an optional parent directory
          2. the size of contained files
          3. a dictionary of named children directories (name is the key)

        We don't have to worry about the name of this directory, because this
        is either stored by its parent, or it is the root directory, in which
        case its name is '/'.
        """
        self.parent = parent
        self._size = 0
        self._children = {}

    def add_child(self, name: str):
        self._children[name] = Directory(self)

    def add_file(self, size: int):
        self._size += size

    def root(self) -> "Directory":
        """Return the root directory.

        Another way to do this would be to set the root's self.parent = self."""
        if self.parent:
            return self.parent.root()
        return self

    @property
    def size(self) -> int:
        """The size of this directory and its children.

        This is implemented as a @property, meaning we can write:
            >>> directory.size
        without an explicit function call. Otherwise we would write:
            >>> directory.size()
        """
        return self._size + sum(c.size for c in self._children.values())

    def find(self, predicate: Callable[["Directory"], bool]) -> Iterator["Directory"]:
        """Find directories matching predicate.

        predicate is a function which takes a directory and returns a bool.
        So we want to call predicate on every directory d (this one and its
        descendents), and collect only the ones for which predicate(d) == True.

        We implement this function recursively, such that self.find(predicate)
        will call child.find(predicate), and collect the results. If we have no
        children (self is a leaf node), we can return immediately. Otherwise,
        we want to combine the value for self with the value of its children.
        Our children will likewise collect the results from their children. In
        this way, we only have to implement one function to handle both the
        "root" case and all descendents.

        We could have implemented this function as:

            results = []
            if predicate(self):
                results.append(self)
            for child in self._children.values():
                results.extend(child.find(predicate))
            return results

        However, this requires a lot of intermediate lists, which are not
        ultimately returned but are only needed to perform the calculation.
        Every call to extend(...) copies the values from one intermediate list
        into another, making it extremely inefficient.

        To avoid this, we use a Python feature called generators, and instead
        of using return we use a related keyword called yield (because we use
        yield, Python automatically makes this method a generator method).

        A generator method returns a generator object g, which is an iterator.
        It hasn't yet executed any of the method body, only created and
        returned g. When we do x = next(g), it executes the method up until the
        first yield statement, which provides the new value of x. When we call
        next(g) again, it picks up where it left off until it hits another
        yield statement.
        """
        if predicate(self):
            yield self
        for child in self._children.values():
            """
            The statement

                yield from it

            is shorthand for

                for x in it:
                    yield x
            """
            yield from child.find(predicate)

    def __getitem__(self, k: str) -> "Directory":
        """Allows us to access a directory's child k by doing directory[k]."""
        return self._children[k]


def process_command(cwd: Directory, command_string: str, remaining_lines: Iterator[str]):
    """Recursively process commands until all input lines are exhausted.

    We use the new match syntax here, but we could equally use a chain of
    if/elif/else statements.

    match "breaks apart" a value. If it is a list l = ["a", "b", "c"], we can
    do:

    match l:
        case 7:
            # match a specific value
        case ["a", "b", "c"]:
            # match list values literally
            ...
        case [x, y, z]:
            # store the values in variables x, y and z
        case [x, y, ...]:
           # use the fist two values and ignore any number of remaining values
        case my_fallback:
           # a single name will match any result, so this is the "default" case

    Only one case can match, processed in order. In this example, the ["a",
    "b", "c"] case would match.

    match works on tuples, dictionaries and user-defined classes - it is
    remarkably powerful but a little bit advanced. In general, in programming
    this technique is called "pattern matching".
    """
    match command_string.split(" "):
        case ["$", "cd", "/"]:
            cwd = cwd.root()
        case ["$", "cd", ".."]:
            cwd = cwd.parent
        case ["$", "cd", target]:
            cwd = cwd[target]
        case ["$", "ls"]:
            return process_ls(cwd, remaining_lines)
        case invalid_command:
            # If none of the cases above match, then something has gone wrong!
            raise ValueError(invalid_command)
    try:
        # Now that we've processed the current command, we should process the
        # next one.
        return process_command(cwd, next(remaining_lines), remaining_lines)
    except StopIteration:
        # next(remaining_lines) will raise a StopIteration exception when there
        # are no lines left. We don't have to do anything, we can just let the
        # function end.
        pass


def process_ls(cwd: Directory, remaining_lines: Iterator[str]):
    """Process results of an ls command before processing remaining commands."""
    for line in remaining_lines:
        match line.split():
            case ["$", *_]:
                # The $ means we've hit a command: process it.
                return process_command(cwd, line, remaining_lines)
            case ["dir", name]:
                cwd.add_child(name)
            case [size, _]:
                cwd.add_file(int(size))
            case invalid_output:
                raise ValueError(invalid_output)


root = Directory()
"""
stdin is a file handle (like opening a file). In Python, file handles are
also iterators, which produce one line of input at a time. So we can write:

    lines = stdin

Each line will end with a '\n' (newline) character. To make things neater, we
want to remove them by calling line.strip(). We could read every line
up-front into a list, and remove the \n values from each one, by doing:

lines = [line.strip() for line in stdin]

(we would then have to do lines = iter(lines) to get an iterator over the
list). But if our file was a million billion lines long, we might run out of
memory when we read them into a list like that. Python provides a builtin
function called map, which takes a function f and an iterator it, and returns
an iterator which defines its next function as:

    def __next__(self):
        f(next(it))

This means that it can modify the values on demand rather than up-front.
This is also called laziness. In general, the following two lines are
equivalent, but the map version is lazy:

    result = [f(x) for x in it]
    result = map(f, it)

Therefore, we map the strip method from str over stdin to get our lines
iterator:
"""
lines = map(str.strip, stdin)
process_command(root, next(lines), lines)
remaining_space = 70_000_000 - root.size
needed_space = 30_000_000
print(sum(d.size for d in root.find(lambda d: d.size <= 100_000)))
print(min(d.size for d in root.find(lambda d: d.size >= needed_space - remaining_space)))

