with open('test_data.txt') as f:
    data = f.read().split('\n')
# 803025030761664
print(data)
# time = int(data[0])
time = 1
my_time = time

busses = data[1].split(',')

while 1:
    step = 1
    for i, bus in enumerate(busses):
        if bus == 'x':
            continue

        bus_id = int(bus)
        if (time + i) % bus_id != 0:
            break
        else:
            step *= bus_id
    else:
        # not breaked loop -> found it
        break

    # try again
    time += step
    continue

print('found:', time)
