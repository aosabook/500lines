from cluster import *
import sys

def key_value_state_machine(state, input_value):
    print input_value, state
    if input_value[0] == 'get':
        return state, state.get(input_value[1], None)
    elif input_value[0] == 'set':
        state[input_value[1]] = input_value[2]
        return state, input_value[2]

def main():
    logging.basicConfig(
        format="%(name)s - %(message)s", level=logging.DEBUG)

    network = Network(int(sys.argv[1]))

    peers = ['N%d' % i for i in range(7)]
    for p in peers:
        node = network.new_node(address=p)
        if p == 'N0':
            Seed(node, initial_state={}, peers=peers, execute_fn=key_value_state_machine)
        else:
            Bootstrap(node, execute_fn=key_value_state_machine, peers=peers).start()

    def request():
        print "here"
    network.set_timer(None, 1.0, request)

    network.run()

if __name__ == "__main__":
    main()
