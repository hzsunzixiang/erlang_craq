-module(read_data_file).

-export([read_file/1, read_all/1, print_entries/1]).

-define(SHA_BYTE_SIZE, 20).
-define(DATA_BIT_SIZE, 32).
-define(HEADER_SIZE, 24).  %% 20 + 4

%% Read and print all records from file
print_entries(FileName) ->
    case read_all(FileName) of
        {ok, Entries} ->
            io:format("~n=== File: ~s ===~n", [FileName]),
            io:format("Total ~p records~n~n", [length(Entries)]),
            lists:foreach(fun({Index, Entry}) ->
                io:format("--- Record #~p ---~n", [Index]),
                print_entry(Entry),
                io:format("~n")
            end, lists:zip(lists:seq(1, length(Entries)), Entries)),
            ok;
        {error, Reason} ->
            io:format("Read failed: ~p~n", [Reason])
    end.

print_entry({eh_storage_data, ObjectType, ObjectId, Timestamp, DataIndex, Status, Column, Value}) ->
    io:format("  ObjectType: ~p~n", [ObjectType]),
    io:format("  ObjectId:   ~p~n", [ObjectId]),
    io:format("  Timestamp:  ~p~n", [Timestamp]),
    io:format("  DataIndex:  ~p~n", [DataIndex]),
    io:format("  Status:     ~p (~s)~n", [Status, status_desc(Status)]),
    io:format("  Column:     ~p~n", [Column]),
    io:format("  Value:      ~p~n", [Value]);
print_entry(Other) ->
    io:format("  ~p~n", [Other]).

status_desc(0) -> "deleted";
status_desc(1) -> "active";
status_desc(_) -> "unknown".

%% Read file and return all records as list
read_all(FileName) ->
    case file:open(FileName, [read, binary, raw]) of
        {ok, File} ->
            Result = read_entries(File, 0, []),
            file:close(File),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% Read single file with detailed info (including offset)
read_file(FileName) ->
    case file:open(FileName, [read, binary, raw]) of
        {ok, File} ->
            Result = read_entries_detail(File, 0, []),
            file:close(File),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% Loop to read all records
read_entries(File, Loc, Acc) ->
    case file:pread(File, Loc, ?HEADER_SIZE) of
        eof ->
            {ok, lists:reverse(Acc)};
        {error, Reason} ->
            {error, Reason};
        {ok, HeaderData} when byte_size(HeaderData) < ?HEADER_SIZE ->
            {ok, lists:reverse(Acc)};  %% Incomplete header, stop
        {ok, <<Sha:?SHA_BYTE_SIZE/binary, BinSize:?DATA_BIT_SIZE>>} ->
            case file:pread(File, Loc + ?HEADER_SIZE, BinSize) of
                eof ->
                    {ok, lists:reverse(Acc)};
                {error, Reason} ->
                    {error, Reason};
                {ok, Data} when byte_size(Data) < BinSize ->
                    {ok, lists:reverse(Acc)};  %% Incomplete data, stop
                {ok, Data} ->
                    %% Verify SHA1
                    ExpectedSha = crypto:hash(sha, <<BinSize:?DATA_BIT_SIZE, Data/binary>>),
                    case Sha =:= ExpectedSha of
                        true ->
                            Entry = binary_to_term(Data),
                            NextLoc = Loc + ?HEADER_SIZE + BinSize,
                            read_entries(File, NextLoc, [Entry | Acc]);
                        false ->
                            io:format("Warning: SHA1 check failed at offset ~p~n", [Loc]),
                            %% Still try to parse
                            Entry = binary_to_term(Data),
                            NextLoc = Loc + ?HEADER_SIZE + BinSize,
                            read_entries(File, NextLoc, [Entry | Acc])
                    end
            end
    end.

%% Read all records with detailed info
read_entries_detail(File, Loc, Acc) ->
    case file:pread(File, Loc, ?HEADER_SIZE) of
        eof ->
            {ok, lists:reverse(Acc)};
        {error, Reason} ->
            {error, Reason};
        {ok, HeaderData} when byte_size(HeaderData) < ?HEADER_SIZE ->
            {ok, lists:reverse(Acc)};
        {ok, <<Sha:?SHA_BYTE_SIZE/binary, BinSize:?DATA_BIT_SIZE>>} ->
            case file:pread(File, Loc + ?HEADER_SIZE, BinSize) of
                eof ->
                    {ok, lists:reverse(Acc)};
                {error, Reason} ->
                    {error, Reason};
                {ok, Data} when byte_size(Data) < BinSize ->
                    {ok, lists:reverse(Acc)};
                {ok, Data} ->
                    ExpectedSha = crypto:hash(sha, <<BinSize:?DATA_BIT_SIZE, Data/binary>>),
                    Valid = (Sha =:= ExpectedSha),
                    Entry = binary_to_term(Data),
                    Detail = #{
                        offset => Loc,
                        size => BinSize,
                        sha_valid => Valid,
                        sha_stored => binary_to_hex(Sha),
                        sha_computed => binary_to_hex(ExpectedSha),
                        entry => Entry
                    },
                    NextLoc = Loc + ?HEADER_SIZE + BinSize,
                    read_entries_detail(File, NextLoc, [Detail | Acc])
            end
    end.

%% Binary to hex string
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).
