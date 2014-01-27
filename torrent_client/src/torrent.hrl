
-record(torrent_info, { tracker_url  :: binary(),
                info_hash    :: binary(),
                name         :: string(),
                piece_length :: pos_integer(),
                total_length :: pos_integer(),
                num_pieces   :: pos_integer(),
                piece_hashes :: binary()
              }).

-record(client, { peer_id    :: binary(),
                  ip         :: inet:ip_address(),
                  port       :: inet:port_number() }).

-define(BLOCK_SIZE, (1 bsl 14)).
