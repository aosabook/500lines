-module(torrent_protocol).

-export([connect/4,accept/3,encode_packet/1,decode_packet/1]).

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

decode_packet(<<0>>) -> {choke, true};
decode_packet(<<1>>) -> {choke, false};
decode_packet(<<2>>) -> {interest, true};
decode_packet(<<3>>) -> {interest, false};
decode_packet(<<4,Have:32>>) -> {have, Have};
decode_packet(<<5,Bits/binary>>) -> {bitfield, bitset:from_binary(Bits)};
decode_packet(<<6,Index:32,Offset:32,Length:32>>) -> {request, Index, Offset, Length};
decode_packet(<<7,Index:32,Offset:32,PieceData/binary>>) -> {block, Index, Offset, PieceData};
decode_packet(<<8,Index:32,Begin:32,Length:32>>) -> {cancel, Index, Begin, Length};
decode_packet(<<>>) -> keep_alive.

encode_packet({choke, true})       -> <<0>>;
encode_packet({choke, false})      -> <<1>>;
encode_packet({interest, true})  -> <<2>>;
encode_packet({interest, false}) -> <<3>>;
encode_packet({have, Index})       -> <<4, Index:32>>;
encode_packet({bitfield, BitSet})  -> Binary = bitset:to_binary(BitSet), <<5, Binary/binary>>;
encode_packet({request, Index, Offset, Length}) -> <<6,Index:32,Offset:32,Length:32>>;
encode_packet({block, Index, Offset, Data}) -> [<<7,Index:32,Offset:32>>, Data];
encode_packet({cancel, Index, Offset, Length}) -> <<8,Index:32,Offset:32,Length:32>>;
encode_packet(keep_alive)          -> <<>>.
