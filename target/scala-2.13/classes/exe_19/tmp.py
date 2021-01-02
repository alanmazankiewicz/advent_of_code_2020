def parse(raw):
    return {i: r.replace('"', '') for i,r in [l.split(': ') for l in raw.splitlines()]}

rules_raw, data_raw = open('input.txt').read().split('\n\n')
rules = parse(rules_raw)

import re
import ring
re_num = re.compile('\d+')


def expand(r: str):
    #print('new call', r)
    pos_dash = r.find('|')
    contains_num = re_num.findall(r)

    if pos_dash > 0:
        return expand(r[:pos_dash]).union(expand(r[pos_dash+1:]))

    if re_num.fullmatch(r.strip()):
        return expand(rules[contains_num[0]])

    if not contains_num and pos_dash < 0:
        return set([r.replace(' ', '')])

    res = set()
    for m in re_num.finditer(r):
        #print(m)
        to_add = set(r[:m.span()[0]] + poss + r[m.span()[1]:] for poss in expand(m.group()))
        #print('to_add', to_add)
        expanded =  [expand(e.strip()) for e in to_add]
        res = res.union(*expanded)
    #print("res", res)

    return res

#print(expand('"a" "b"'))
all_rules = expand('0')

sum(1 for m in data_raw.splitlines() if m in all_rules)

len(data_raw.splitlines())


expand.cache_clear()
all_rules = expand('0')
four2 = expand('42')
three1 = expand('31')

len(three1)

already_good = sum(1 for m in data_raw.splitlines() if m in all_rules)
to_check = [m for m in data_raw.splitlines() if m not in all_rules]
print(len(to_check), already_good)




def get_31s(d):
    #print('call', d)
    p = set(d[len(c):] for c in three1 if d.startswith(c))
    if len(p) == 0:
        return set([d])
    for a in p:
        return set('31 ' + b for b in get_31s(a))
get_31s.cache_clear()


def is_valid(d: str):
    d = d.strip()
    s = set(d.split(' '))
    if not s in [set(['31', '42']), set(['42'])]:
        return False
    if not d.startswith('42'):
        return False
    if d.count('42 31') != 1:
        return False
    if d.count('31 42') > 0:
        return False
    if d.count('31') >= d.count('42'):
        return False
    #print(d)
    return True


possibles = []
for c in to_check:
    res = set()
    for cand in get_42s(c):
        spl = cand.strip().split(' ')
        for replaced in get_31s(spl[-1]):
            res.add(" ".join(spl[:-1] + [replaced.strip()]))
        #res.update()

    possibles.append(res)

res = 0
for i,el in enumerate(possibles):
    #print(el)
    if any(map(is_valid, el)):
        # print(el, to_check[i])
        res += 1

print(res + already_good)


