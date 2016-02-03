# A Torrent Client

## How The Torrent Protocol Works

To share a file using the torrent system, first someone has to construct a `.torrent` file that describes the content of the file being shared.  It uses a kind of binary JSON format called bencode.  The contents of the `.torrent` file describes the file name, size, block size, and a list of SHA1-hash values for each block of data in the file.  The block size is decided by whoever creates the torrent file, but it should be bigger than 32k, and also so large that there are no more than ~4000 block all in all.  Since each SHA1-hash is 20 bytes, this leaves the resulting torrent file comfortably below 100k, which is the recommended maximum.  The SHA1-hash of the torrent file itself (the hash of hashes) is called the *Info Hash*, and is used to identify the file being shared as a whole.
 
A set of running torrent clients which all have a copy of the same torrent file is called a *swarm*.  Each agent in the swarm is called a *peer*.  Some peers will have all the blocks (called *seeders*), and some will just have a subset (called *leechers*).  The shared goal of the swarm is to help all the peers to get a complete copy of the original file.

> Now, you might say "why not just download the file from the original seeder?"  

> The issue is that if a million people want to download a 400GB disk image, then your DSL line might get swamped quite quickly.  So, the idea is to share it with just a few peers, assuming they will also also share it with a few other peers, and quickly this leaves everyone better off.  Not only does this leave the original seeder off a huge bill from her ISV, it also means that the downloaders may be able to ultimately download most of the file from a closer peer, yielding better download speed.
 
> For instance, if you are making Linux distributions, all your downloaders can cooperate to download quicker, all the while saving on the bill you need to pay your ISV.
 
To share the file with anyone however, you need to know who is part of the swarm, and for this you need a *tracker*.  A tracker a web server that all the peers will contact to register, and ask who else is part of the swarm for a given torrent file as identified by the info hash.  The tracker doesn't even need to know the contents of the `.torrent` file, but just keeps a list of *peer-id* (a 20-byte random number) and *host:port* for all the peers interested in a given info hash.  The URL of the tracker is also included in the `.torrent` file.

>There are other ways to form swarms than using a HTTP-based tracker, one of which is known as DHT (for Distributed Hash Table).  Except for this paragraph, we'll only cover HTTP based trackers.  
>DHT work by forming a global network of all torrent peers in the world, and using them much like we use DNS.  In the DHT network, all nodes participate as routers for the peers it knows, and as trackers for info hashes that "close to" the node's own peer id (for some definition of "close to"). 

Ideally, all the peers in a swarm can accept connections on the public internet, so that other peers can contact them.  The peers that can accept connections are at an advantage because they can get more peers, but the system will work fine if just some peers are accessible on the internet.   Once connected, the peer to peer relationship is completely symmetrical, as is the protocol they speak to each other.

To reduce complexity and code size, this torrent client does not accept connections from the internet.  Also, since you're likely to be running the examples from behind a firewall, it may not always work.

## A Few Notes on Reading Erlang

I'll assume that you don't know Erlang, and so here are a few hints to how to read the code that will come ahead.  If you're new to Erlang, please take some time to understand these rules, and you'll be much happier going forward:

- Expressions that are evaluated in sequence are separated by commas.  Semi-colons are used in between the individual cases of a `case` statement, of which erlang has several kinds.  Function definitions (and other top-level declarations) are terminated with a period.  This is consistenly employed, but somewhat unorthodox.  Take your time to locate the semi-colon and the period in this code defining a speed function:

````erlang
speed(Time, Distance) ->
  case Time of
    0 -> io:format("very fast!\n"), 
         infinity;
    _ -> Distance / Time
  end.
````
- Unsurprisingly, code is organized in modules, and in order to call a function in another module you write `module:function(...)`.  When calling local function, you don't need to prefix the function call with the module name.  The "return value" of a function is simply the value of the last expression of its evaluation; there is no `return Foo` statement.
- A constant string is called an *atom* in Erlang, and usually looks like an all-lowercase identifier `like_this`.  Atoms are used as you would normally use enums in other languages: they are globally unique, and so they're fast to compare for equality.  Regardless of it's resemblance with identifiers in most languages it is not an identifier, it's just a constant string!  You can enclose it in single quotes if you want `'like_this'`, but that is only useful for atoms containing non-lowercase letters.  In the above code, `infinity` is an atom. In fact, in Erlang the boolean values `true` and `false` are also just atoms.   
- Variables are identifiers that start with a capital letter, `LikeThis`.  That's quite unlike languages like Java and Ruby, where uppercase identifiers signal constant-ness.
- All assignments are final; so a variable cannot change.  As such, they are not really variables, but that's a discussion I won't take here.  Becaus of this single-assignment rule, you will often see the same variable name with 1, 2, or 3 attached, such as `State1` or `State2` to signify different "versions" of the same value. 
- Tuples `{1,2,3}` are widely used for simple structures.  It's a common idiom to return a 2-tuple with an `ok` atom in the first position, such as `{ok, Value}` for success, and `{error, Reason}` for failure.
- All assignments use desctructuring (pattern matching), so the assignment below succeeds iff the `foo` function returns a 2-tuple, in which the first element is the atom `ok`.  The variable `Value` is assigned whatever was in the second position of the returned tuple.

````erlang
{ok, Value} = foo(1,2,3)
````

- The special identifier `_` (underbar) always matches, and discards the value.  
- If you assign a value to a "variable" that has already been assigned, then it will only suceed if the new value is equal to the old one.  

Enough, let's dive in!

## The Main Logic of the Code

The code is organized in a number of modules, of which two has special interest to understand how it operates: `torrent_client` and `torrent_peer`.  Both of these modules have a `main_loop` function that runs an infinite loop. We will have 

- one process (thread) for each `.torrent` file being shared executing the `torrent_client:main_loop` function, and 
- one process executing the `torrent_peer:main_loop` function for each remote peer we're connected to with a tcp socket.

In Erlang, a function can execute in an infinite loop by calling itself without causing the stack to run full, if the self-recursive call appears as the *last* expression of the function, in so-called *tail position*.  This is because a self-call at that place can discard all the local variables (the activation record) since those would not be available anyway, and just do the call using a `goto` operation on the underlying virtual machine.


### Client Process

The "client" knows which parts we have, and which parts we want (two disjoint sets of block numbers, of which the union is the full set of block numbers).  Every time a peer completes a part download it sends a `{downloaded, PartNo}` message to the client process, which updates the have/want sets and broadcasts `{finished, PartNo}` to all the peer processes.   Erlang's `receive` construct is a blocking operation that waits for a message matching one of the enclosed clauses. Thus, the gist of the client's main loop is this:

````erlang
main_loop(State=#state{ peers=Peers, have=Have, missing = Missing }) ->
    receive
        {downloaded, PieceNo} ->
            State2=State#state{ 
                   missing = ordsets:del_element(PieceNo, Missing),
                   have    = ordsets:add_element(PieceNo, Have) },
            case State2#state.missing == [] of
                true  ->
                    [ erlang:exit(PeerPID, shutdown) || PeerPID <- Peers ],
                    io:format("download done!~n"),
                    ok;
                false ->
                    [ PeerPID ! {finished, PieceNo} || PeerPID <- Peers ],
                    main_loop(State2)
            end;
    ...
````

### Peer Process

For each connected peer, there is a process that sits in an infinite loop waiting for incoming messages.  The peer waits for

- Data from the socket.  Incoming data arrives as a `{tcp, Socket, Packet}` where `Packet` is the actual data received.   
- The 10-second interval timer triggering,
- Notifications from the manager that some other peer has successfully finished downloading some piece

The main loop is depicted here:

````erlang
main_loop(State=#state{ sock=Sock, upload_allowance=Allowance, want=Want, have=Have }) ->
  receive
    {tcp, Sock, Packet} ->
       Message = torrent_protocol:decode_packet(Packet, Info),
       inet:setopts(Sock, [{active,once}]),
       handle_incoming(Message, State);

    {timeout, _, ten_sec_timer} ->
      Timer = erlang:start_timer(10000, self(), ten_sec_timer),
      {ok, State2} = send(keep_alive, State),
      do_work(State2#state{ 
            upload_allowance=?UPLOAD_BYTES_PER_10SECOND), 
            timer=Timer });

    {finished, PieceNo} ->
      case ordsets:is_element(PieceNo, Want) of
        true  -> 
            % cancel any outstanding requests
            {ok, State2} = cancel(PieceNo, State);
        false -> 
            % inform peer that we can now provide PieceNo
            {ok, State2} = send({have, PieceNo}, State)
      end,
      do_work(State2#state{ 
                have=ordsets:add_element(PieceNo, Have),
                want=ordsets:del_element(PieceNo, Want) });
      ...
````
