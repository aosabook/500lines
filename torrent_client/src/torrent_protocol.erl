-module(torrent_protocol).

-export([connect/4,accept/3,encode_packet/2,decode_packet/2]).

-define(HANDSHAKE(Flags,InfoHash,PeerID), <<19, "BitTorrent protocol", Flags : 64, InfoHash:20/binary, PeerID:20/binary>>).

connect(Host, Port, ClientID, InfoHash) ->
    case gen_tcp:connect(Host, Port, [], 1000) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, ?HANDSHAKE(0, InfoHash, ClientID)),
            ok = inet:setopts(Sock, [{packet, raw}, {active, false}, binary]),
            case gen_tcp:recv(Sock, 68) of
                {ok, ?HANDSHAKE(_,InfoHash,PeerID)} ->
                    {ok, {Sock, PeerID}};
                _Got ->
                    gen_tcp:close(Sock),
                    {error, {bad_handshake, _Got}}
            end;
        Error ->
            Error
    end.

accept(ListenSock, ClientID, AcceptFun) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    case gen_tcp:recv(Sock, 68) of
        {ok, ?HANDSHAKE(_,InfoHash,PeerID)} ->
            case AcceptFun(InfoHash) of
                {ok, OwnerPID} ->
                    ok = gen_tcp:send(Sock, ?HANDSHAKE(0,InfoHash,ClientID)),
                    inet:setopts(Sock, [{packet,4}, binary]),
                    {ok, {Sock, OwnerPID, PeerID}};
                false ->
                    gen_tcp:close(Sock),
                    accept(ListenSock, ClientID, AcceptFun)
            end;
        _ ->
            gen_tcp:close(Sock),
            accept(ListenSock, ClientID, AcceptFun)
    end.

decode_packet(<<0>>, _Info) -> {choke, true};
decode_packet(<<1>>, _Info) -> {choke, false};
decode_packet(<<2>>, _Info) -> {interest, true};
decode_packet(<<3>>, _Info) -> {interest, false};
decode_packet(<<4,Have:32>>, _Info) -> {have, Have};
decode_packet(<<5,Bits/binary>>, _Info) -> {bitfield, bitset:from_binary(Bits)};
decode_packet(<<6,Index:32,Offset:32,Length:32>>, _Info) -> {request, Index, Offset, Length};
decode_packet(<<7,Index:32,Offset:32,PieceData/binary>>, _Info) -> {block, Index, Offset, PieceData};
decode_packet(<<8,Index:32,Begin:32,Length:32>>, _Info) -> {cancel, Index, Begin, Length};
decode_packet(<<>>, _Info) -> keep_alive.

encode_packet({choke, true}, _Info)       -> <<0>>;
encode_packet({choke, false}, _Info)      -> <<1>>;
encode_packet({interest, true}, _Info)  -> <<2>>;
encode_packet({interest, false}, _Info) -> <<3>>;
encode_packet({have, Index}, _Info)       -> <<4, Index:32>>;
encode_packet({bitfield, BitSet}, _Info)  -> Binary = bitset:to_binary(BitSet), <<5, Binary/binary>>;
encode_packet({request, Index, Offset, Length}, _Info) -> <<6,Index:32,Offset:32,Length:32>>;
encode_packet({block, Index, Offset, Data}, _Info) -> [<<7,Index:32,Offset:32>>, Data];
encode_packet({cancel, Index, Offset, Length}, _Info) -> <<8,Index:32,Offset:32,Length:32>>;
encode_packet(keep_alive, _Info)          -> <<>>.
