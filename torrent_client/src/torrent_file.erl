-module(torrent_file).

-include("torrent.hrl").

-export([read/1,piece_offset/2,piece_sha/2,piece_length/2]).

read(TorrentFileName) ->
  {ok, TorrentData} = file:read_file(TorrentFileName),
  {ok, TorrentDict} = bencode:decode(TorrentData),
  {ok, InfoDict}    = bencode:find(TorrentDict, info),
  {ok, Encoded}     = bencode:encode(InfoDict),
  {ok, Length}      = bencode:find(InfoDict, length),
  {ok, Name}        = bencode:find(InfoDict, name),
  FileName          = unicode:characters_to_list(Name, utf8),
  {ok, PieceLength} = bencode:find(InfoDict, 'piece length'),
  {ok, PieceHashes} = bencode:find(InfoDict, pieces),
  {ok, TrackerURL}  = bencode:find(TorrentDict, announce),
  PieceCount        = byte_size(PieceHashes) div 20,
  InfoHash          = crypto:hash(sha, Encoded),
  #torrent_info{ info_hash=InfoHash, tracker_url=TrackerURL, name=FileName,
  total_length=Length, piece_length=PieceLength, num_pieces=PieceCount,
  piece_hashes=PieceHashes
  }.


piece_sha(N, #torrent_info{ piece_hashes=Hashes }) ->
  BeforeBytes = N*20,
  <<_:BeforeBytes/binary, SHA:20/binary, _/binary>> = Hashes,
  SHA.

piece_offset(N, #torrent_info{ piece_length=PL }) ->
  N*PL.

piece_length(P, #torrent_info{ piece_length=PL, num_pieces=N, total_length=TL }) when (P+1)==N ->
  TL - (P*PL);
piece_length(P, #torrent_info{ piece_length=PL, num_pieces=N }) when P < N ->
  PL.

