import sys
import logging
from fleet import Ship


def key_value_state_machine(state, input_value):
    print input_value, state
    if input_value[0] == 'get':
        return state, state.get(input_value[1], None)
    elif input_value[0] == 'set':
        state[input_value[1]] = input_value[2]
        return state, input_value[2]


def main():
    logging.basicConfig(
        format="%(asctime)s - %(name)s - %(message)s", level=logging.WARNING)

    if sys.argv[1] == '--seed':
        sys.argv.pop(1)
        seed = {}
    else:
        seed = None

    ship = Ship(state_machine=key_value_state_machine,
                port=int(sys.argv[1]), peers=['127.0.0.1-%s' % p for p in sys.argv[2:]],
                seed=seed)
    ship.start()

    while True:
        command = raw_input("fleet> ").split()
        if command[0] == 'set':
            ship.invoke(('set', command[1], command[2]))
        elif command[0] == 'get':
            print "got", ship.invoke(('get', command[1]))


if __name__ == "__main__":
    main()
