import sys
from collections import defaultdict

import numpy as np
from more_itertools import first


TILE_SIZE = 4
char_array = [[c for c in l.rstrip()] for l in sys.stdin][:-2]

width, height = max(len(row) for row in char_array), len(char_array)

face_coords = {(x, y) for y in range(height // TILE_SIZE) for x in range(width // TILE_SIZE)
 if x*TILE_SIZE < len(char_array[y*TILE_SIZE]) and char_array[y*TILE_SIZE][x*TILE_SIZE] != ' '}

DIRECTIONS = {
    "NORTH": (0, -1),
    "EAST": (1, 0),
    "SOUTH": (0, 1),
    "WEST": (-1, 0)
}

first_face = first(face_coords)

frontier = {first_face}
normals = {first_face: np.array([0, 0, 1])}
bases = {first_face: np.array([[1, 0, 0], [0, 1, 0]])}

adjacent_normals = defaultdict(dict)

while frontier:
    face = frontier.pop()
    normal = normals[face]
    basis = bases[face]
    for dirname, delta2d in DIRECTIONS.items():
        delta3d = np.squeeze(np.array(delta2d) @ basis)
        adjacent_normals[face][dirname] = delta3d
        if (new_face := tuple(np.add(face, delta2d))) in face_coords and new_face not in bases:
            # 90 degree rotation of basis performed with Rodriguez' formula
            I = np.eye(3, dtype=int)
            a = np.cross(np.cross(normal, delta3d), -I)
            bases[new_face] = basis @ (I + a + a@a).T
            normals[new_face] = delta3d
            frontier.add(new_face)

adjacent_dirs = {face: {tuple(normal): dirname for dirname, normal in dirs.items()}
                 for face, dirs in adjacent_normals.items()}
faces = {tuple(normal): face for face, normal in normals.items()}

resolved_edges = {}
for face_a, dirs in adjacent_normals.items():
    normal_a = normals[face_a]
    for dir_a, normal_b in dirs.items():
        face_b = faces[tuple(normal_b)]
        dir_b = adjacent_dirs[face_b][tuple(normal_a)]
        resolved_edges[(face_a, dir_a)] = (face_b, dir_b)

for k, v in resolved_edges.items():
    print(k, v)
