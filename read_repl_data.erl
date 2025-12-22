-module(read_repl_data).
-export([run/0, run/1, read/1, print/1]).

-define(SHA_BYTE_SIZE, 20).
-define(DATA_BIT_SIZE, 32).
-define(HEADER_SIZE, 24).  %% 20 + 4

%% Default file name
-define(DEFAULT_FILE, "ec_n1_repl.data.0000000001").

%% Quick run with default file
run() ->
    print(?DEFAULT_FILE).

%% Run with specified file
run(FileName) ->
    print(FileName).

%% Read and print all records
print(FileName) ->
    case read(FileName) of
        {ok, Entries} ->
            io:format("~n=== File: ~s ===~n", [FileName]),
            io:format("Total: ~p records~n~n", [length(Entries)]),
            print_entries(Entries, 1);
        {error, Reason} ->
            io:format("Read failed: ~p~n", [Reason])
    end.

print_entries([], _) -> ok;
print_entries([Entry | Rest], Index) ->
    io:format("#~p: ", [Index]),
    print_entry(Entry),
    print_entries(Rest, Index + 1).

print_entry({eh_storage_data, ObjectType, ObjectId, Timestamp, DataIndex, Status, Column, Value}) ->
    StatusStr = status_desc(Status),
    io:format("{~p, ~p} ts=~p idx=~p status=~s col=~p val=~p~n", 
              [ObjectType, ObjectId, Timestamp, DataIndex, StatusStr, Column, Value]);
print_entry(Other) ->
    io:format("~p~n", [Other]).

status_desc(0) -> "del";
status_desc(1) -> "act";
status_desc(_) -> "unk".

%% Read file and return all records
read(FileName) ->
    case file:open(FileName, [read, binary, raw]) of
        {ok, File} ->
            Result = read_loop(File, 0, []),
            file:close(File),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

read_loop(File, Loc, Acc) ->
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
                    %% Verify SHA1: hash(<<Size:32, Data/binary>>)
                    ExpectedSha = crypto:hash(sha, <<BinSize:?DATA_BIT_SIZE, Data/binary>>),
                    Valid = (Sha =:= ExpectedSha),
                    case Valid of
                        false ->
                            io:format("Warning: SHA1 mismatch at offset ~p~n", [Loc]);
                        true ->
                            ok
                    end,
                    Entry = binary_to_term(Data),
                    NextLoc = Loc + ?HEADER_SIZE + BinSize,
                    read_loop(File, NextLoc, [Entry | Acc])
            end
    end.

