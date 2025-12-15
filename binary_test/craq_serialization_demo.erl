%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CRAQ Serialization and Deserialization Demo
%% Complete simulation of eh_storage_data_api functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(craq_serialization_demo).
-export([demo/0]).

%% Constants from erlang_craq.hrl
-define(STATUS_INACTIVE, 0).
-define(STATUS_ACTIVE, 1).
-define(EH_BAD_DATA, eh_bad_data).

%% Constants from eh_storage_data_api.erl
-define(SHA_BYTE_SIZE, 20).
-define(DATA_BIT_SIZE, 32).

%% Record definition from erlang_craq.hrl
-record(eh_storage_data, {
    object_type :: atom(),
    object_id :: term(),
    timestamp :: non_neg_integer(),
    data_index :: non_neg_integer(),
    status = ?STATUS_ACTIVE :: ?STATUS_ACTIVE | ?STATUS_INACTIVE,
    column :: atom(),
    value :: term()
}).

%% Main demo function
demo() ->
    io:format("=== CRAQ Serialization & Deserialization Demo ===~n~n"),
    
    %% 1. Create test data - simulating CRAQ storage data
    TestData = #eh_storage_data{
        object_type = person,
        object_id = 10,
        timestamp = 1,
        data_index = 1,
        status = ?STATUS_ACTIVE,
        column = name,
        value = john
    },
    
    io:format("1. Original Data Structure:~n"),
    io:format("   ~p~n~n", [TestData]),
    
    %% 2. Step-by-step serialization process
    io:format("2. Serialization Process:~n"),
    
    %% Step 2a: Erlang native serialization
    RawBinary = term_to_binary(TestData),
    RawSize = byte_size(RawBinary),
    io:format("   Step 2a - term_to_binary():~n"),
    io:format("     Size: ~p bytes~n", [RawSize]),
    io:format("     Binary: ~p~n", [binary_to_list(RawBinary)]),
    
    %% Step 2b: Hash calculation
    HashInput = <<RawSize:?DATA_BIT_SIZE, RawBinary:RawSize/binary>>,
    Hash = crypto:hash(sha, HashInput),
    io:format("   Step 2b - SHA1 Hash Calculation:~n"),
    io:format("     Hash input: [4-byte size] + [binary data]~n"),
    io:format("     SHA1 result: ~p~n", [Hash]),
    
    %% Step 2c: Final binary assembly
    FinalBinary = <<Hash:?SHA_BYTE_SIZE/binary, RawSize:?DATA_BIT_SIZE, RawBinary:RawSize/binary>>,
    io:format("   Step 2c - Final Binary Assembly:~n"),
    io:format("     Format: [20-byte hash] + [4-byte size] + [data]~n"),
    io:format("     Total size: ~p bytes~n", [byte_size(FinalBinary)]),
    io:format("     Structure: SHA1(~p) + Size(~p) + Data(~p)~n~n", 
              [?SHA_BYTE_SIZE, 4, RawSize]),
    
    %% 3. File operations
    io:format("3. File Operations:~n"),
    FileName = "demo_storage.bin",
    file:write_file(FileName, FinalBinary),
    io:format("   Saved to file: ~s~n", [FileName]),
    
    {ok, LoadedBinary} = file:read_file(FileName),
    io:format("   Loaded from file: ~p bytes~n", [byte_size(LoadedBinary)]),
    io:format("   Data integrity: ~p~n~n", [FinalBinary =:= LoadedBinary]),
    
    %% 4. Deserialization process
    io:format("4. Deserialization Process:~n"),
    
    %% Step 4a: Binary parsing
    <<LoadedHash:?SHA_BYTE_SIZE/binary, 
      LoadedSize:?DATA_BIT_SIZE,
      LoadedData/binary>> = LoadedBinary,
    ActualData = binary:part(LoadedData, 0, LoadedSize),
    
    io:format("   Step 4a - Binary Parsing:~n"),
    io:format("     Extracted hash: ~p~n", [LoadedHash]),
    io:format("     Extracted size: ~p~n", [LoadedSize]),
    io:format("     Data length: ~p~n", [byte_size(ActualData)]),
    
    %% Step 4b: Hash verification
    VerifyHashInput = <<LoadedSize:?DATA_BIT_SIZE, ActualData:LoadedSize/binary>>,
    VerifyHash = crypto:hash(sha, VerifyHashInput),
    HashMatch = (LoadedHash =:= VerifyHash),
    
    io:format("   Step 4b - Hash Verification:~n"),
    io:format("     Computed hash: ~p~n", [VerifyHash]),
    io:format("     Hash match: ~p~n", [HashMatch]),
    
    %% Step 4c: Data reconstruction
    if HashMatch ->
        ReconstructedData = binary_to_term(ActualData),
        io:format("   Step 4c - Data Reconstruction:~n"),
        io:format("     Success: ~p~n", [ReconstructedData]),
        io:format("     Original match: ~p~n~n", [TestData =:= ReconstructedData]);
    true ->
        io:format("   Step 4c - Data Reconstruction:~n"),
        io:format("     Failed: Hash mismatch - data corrupted~n~n")
    end,
    
    %% 5. Performance analysis
    io:format("5. Performance Analysis (10000 operations):~n"),
    
    {SerTime, _} = timer:tc(fun() ->
        [begin
            Bin = term_to_binary(TestData),
            Size = byte_size(Bin),
            H = crypto:hash(sha, <<Size:?DATA_BIT_SIZE, Bin:Size/binary>>),
            <<H:?SHA_BYTE_SIZE/binary, Size:?DATA_BIT_SIZE, Bin:Size/binary>>
        end || _ <- lists:seq(1, 10000)]
    end),
    
    {DeserTime, _} = timer:tc(fun() ->
        [begin
            <<H:?SHA_BYTE_SIZE/binary, S:?DATA_BIT_SIZE, D/binary>> = FinalBinary,
            Data = binary:part(D, 0, S),
            VerH = crypto:hash(sha, <<S:?DATA_BIT_SIZE, Data:S/binary>>),
            case H =:= VerH of
                true -> binary_to_term(Data);
                false -> ?EH_BAD_DATA
            end
        end || _ <- lists:seq(1, 10000)]
    end),
    
    {PureSerTime, _} = timer:tc(fun() ->
        [term_to_binary(TestData) || _ <- lists:seq(1, 10000)]
    end),
    
    io:format("   Full serialization: ~.2f μs/op~n", [SerTime/10000]),
    io:format("   Full deserialization: ~.2f μs/op~n", [DeserTime/10000]),
    io:format("   Pure term_to_binary: ~.2f μs/op~n", [PureSerTime/10000]),
    io:format("   Hash overhead: ~.2f μs/op~n~n", [(SerTime-PureSerTime)/10000]),
    
    %% 6. Corruption detection demo
    io:format("6. Corruption Detection Demo:~n"),
    
    %% Corrupt the last byte
    CorruptedBinary = binary:replace(FinalBinary, 
                                   binary:part(FinalBinary, byte_size(FinalBinary)-1, 1), 
                                   <<255>>),
    
    <<CorruptHash:?SHA_BYTE_SIZE/binary, 
      CorruptSize:?DATA_BIT_SIZE,
      CorruptData/binary>> = CorruptedBinary,
    CorruptActualData = binary:part(CorruptData, 0, CorruptSize),
    
    CorruptVerifyHash = crypto:hash(sha, <<CorruptSize:?DATA_BIT_SIZE, CorruptActualData:CorruptSize/binary>>),
    CorruptMatch = (CorruptHash =:= CorruptVerifyHash),
    
    io:format("   Corrupted last byte~n"),
    io:format("   Hash verification: ~p~n", [CorruptMatch]),
    if CorruptMatch ->
        io:format("   ✗ Failed to detect corruption~n");
    true ->
        io:format("   ✓ Successfully detected corruption~n")
    end,
    
    %% Cleanup
    %%file:delete(FileName),
    io:format("~n7. Demo completed successfully!~n").
