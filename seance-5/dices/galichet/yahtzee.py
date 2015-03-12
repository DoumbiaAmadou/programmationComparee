# imports
import random

# constants
Face = 6
Min = 1
Max = Face + 1
Lancer = 5

def is_full(full):
    l = sorted(full)
    return ((l[0] == l[1])
                and ((l[1] == l[2])
                    or (l[2] == l[3]))
                and (l[3] == l[4]))

def is_suite(suite):
    l = sorted(suite)
    return ((l[0] < l[1])
                and (l[1] < l[2])
                and (l[2] < l[3])
                and (l[3] < l[4]))


def full():
    v1 = random.randint(Min, Max)
    v2 = filter(lambda x : x != v1 ,range(Min, Max))
    v3 = random.sample(v2, 1)
    f = [v1,v1,v1,v3[0],v3[0]]
    for i in range(random.randint(0,100)):
        random.shuffle(f)
    return f

def suite():
    n = random.randint(0, Face - Lancer)
    s = [i+n for i in range(Lancer)]
    for i in range(random.randint(0,100)):
        random.shuffle(s)
    return s







#if __name__ == '__main__':
