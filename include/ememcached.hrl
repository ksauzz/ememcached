% TODO: only sock?
-record(state, {port::integer(), lsock::gen_tcp:socket()}).
-record(ememcached_record,
   {key::string(),
    flags::integer(),
    exptime::integer(),
    created_datetime::integer(),
    bytes::integer(),
    data_block::binary()}).

