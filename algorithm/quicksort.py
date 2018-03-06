from operator import gt, le
from functools import partial
from itertools import permutations


qs1 = lambda l: qs1([*filter(partial(gt, l[0]), l[1:])]) + l[0:1] + qs1([*filter(partial(le, l[0]), l[1:])]) if l else []
qs2 = lambda l: qs2([i for i in l[1:] if i <= l[0]]) + l[0:1] + qs2([i for i in l[1:] if i > l[0]]) if l else []
qs_l = [v for k, v in vars().items() if 'qs' in k]

def test():
    crt = list(range(5))
    cases = map(list, permutations(list(range(5))))
    for f in qs_l:
        for i in cases:
            assert f(i) == crt
            print("pass")
