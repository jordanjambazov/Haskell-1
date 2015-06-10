import operator

from shortestpath import dijkstra


WEIGHT = 5
START_NODE = "H"
END_NODE = "L"


def flatten(path):
    pre = ""
    if path[1]:
        pre = flatten(path[1])
    return pre + path[0]


def get_input_and_edges():
    print("Please enter train routes (on each line)")
    empty = False
    edges = []
    while True:
        route = raw_input()
        empty = route == ""
        if empty:
            break
        edge = tuple(route.split() + [WEIGHT])
        edges.append(edge)
    return edges



def main():
    _, path = dijkstra(get_input_and_edges(), START_NODE, END_NODE)
    print(" ".join(flatten(path)))


if __name__ == "__main__":
    main()
