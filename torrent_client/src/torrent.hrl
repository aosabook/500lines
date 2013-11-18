
-record(info, { tracker_url  :: string(),
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

-define(MAX_INFLIGHT_REQUESTS, 10).
-define(BLOCK_SIZE, (1 bsl 14)).
-define(UPLOAD_BYTES_PER_SECOND, 1024 * 50).
