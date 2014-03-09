import sys
import logging
from fleet import Ship


def key_value_state_machine(state, input):
    print input, state
    if input[0] == 'get':
        return state, state.get(input[1], None)
    elif input[0] == 'set':
        state[input[1]] = input[2]
        return state, input[2]


def main():
    logging.basicConfig(format="%(asctime)s - %(name)s - %(message)s", level=logging.WARNING)

    if sys.argv[1] == '--seed':
        sys.argv.pop(1)
        seed = {}
    else:
        seed = None

    ship = Ship(state_machine=key_value_state_machine,
                port=int(sys.argv[1]), peers=['127.0.0.1-%s' % p for p in sys.argv[2:]],
                seed=seed)
    ship.start()

    for event in ship.events():
        print event
        old = ship.invoke(('get', sys.argv[1])) or 0
        print "got", old
        ship.invoke(('set', sys.argv[1], old + 1))


if __name__ == "__main__":
    main()
