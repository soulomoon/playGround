from operator import gt, le
from functools import partial, reduce
from itertools import permutations


def exchange(l):
    if len(l) > 1 and l[0] > l[1]:
        l[0], l[1] = l[1], l[0]


def sround(l, *_):
    exchange(l)
    return l[0:1] + sround(l[1:]) if l else l


def bubblesort(l): return reduce(sround, range(len(l)), l)


def bb_inplace(l):
    for i in range(len(l)):
        for j in range(len(l)-1):
            if l[j] > l[j+1]:
                l[j], l[j+1] = l[j+1], l[j]


if __name__ == "__main__":
    crt = list(range(5))
    cases = list(map(list, permutations(list(range(5)))))
    for i in cases:
        assert bubblesort(i) == crt
        assert bb_inplace(i) == crt

    print("test done total: {}".format(len(cases)))
