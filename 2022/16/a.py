#!/usr/bin/env python3
import sys
from typing import Iterable, Iterator, Tuple
from itertools import chain, combinations

Path = frozenset[str]


def main(lines: Iterable[str]):
    adjacency = {}
    rates = {}
    for line in map(str.split, lines):
        key, rate, rest = line[1], line[4], line[9:]
        adjacency[key] = [r.rstrip(",") for r in rest]
        rates[key] = int(rate[5:-1])

    rates = {k: v for k, v in rates.items() if v > 0}

    all_pairs = transitive_closure(adjacency)
    print(max(score for _, score in all_paths(all_pairs, rates, "AA")))
    print(maximum_pair(all_paths(all_pairs, rates, "AA", max_cost=26, early_exit=True))[1])


def maximum_pair(paths: Iterable[Tuple[Path, int]]) -> Tuple[Tuple[Path, Path], int]:
    maxed_paths = {}
    for path, score in paths:
        maxed_paths[path] = max(maxed_paths.get(path, score), score)
    path_pairs = sorted((((p1, p2), s1+s2) for (p1, s1), (p2, s2) in combinations(maxed_paths.items(), 2)),
                        key=lambda pair: -pair[1])
    return next(filter(lambda k: not frozenset.intersection(*k[0]), path_pairs))


def all_paths(costs, rates, start_node, max_cost=30, early_exit=False) -> Iterator[Tuple[Path, int]]:
    def go(current_node, visited, score, remaining_cost):
        next_cost = lambda n: remaining_cost - costs[current_node][n] - 1
        next_nodes = [(k, next_cost(k)) for k in rates if k not in visited and next_cost(k) >= 0]
        yield from chain.from_iterable(
            (go(k, visited | {k}, score + cost*rates[k], cost)) for k, cost in next_nodes)
        if early_exit or not next_nodes:
            yield (visited, score)
    yield from go(start_node, frozenset(), 0, max_cost)


def transitive_closure(adjacency: dict[str, list[str]]) -> dict[str, dict[str, int]]:
    def go(working_set: set[str], costs, cost=0):
        if working_set:
            costs |= {node: cost for node in working_set}
            return go(set(chain.from_iterable(
                {n for n in adjacency[current_node] if n not in costs}
                for current_node in working_set
            )), costs, cost+1)
        return costs
    return {k: go({k}, {}) for k in adjacency}


if __name__ == "__main__":
    main(sys.stdin)
