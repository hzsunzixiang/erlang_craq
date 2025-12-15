%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CRAQ序列化和反序列化测试模块
%% 模拟eh_storage_data_api的完整功能
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(craq_serialization_test).
-export([test_serialization/0, 
         entry_to_binary/1, 
         binary_to_entry/2,
         compute_hash/2,
         save_to_file/2,
         load_from_file/1,
         test_corruption_detection/0]).

%% 常量定义 - 参考erlang_craq.hrl
-define(STATUS_INACTIVE,           0).
-define(STATUS_ACTIVE,             1).
-define(EH_BAD_DATA,               eh_bad_data).

%% 常量定义 - 参考eh_storage_data_api.erl
-define(SHA_BYTE_SIZE,         20).
-define(DATA_BIT_SIZE,         32).

%% 记录定义 - 参考erlang_craq.hrl
-record(eh_storage_data,        {object_type                         :: atom(),
                                 object_id                           :: term(),
                                 timestamp                           :: non_neg_integer(),
                                 data_index                          :: non_neg_integer(),
                                 status=?STATUS_ACTIVE               :: ?STATUS_ACTIVE | ?STATUS_INACTIVE,
                                 column                              :: atom(),
                                 value                               :: term()}).

%% 主测试函数
test_serialization() ->
    io:format("=== CRAQ序列化和反序列化测试 ===~n~n"),
    
    %% 1. 创建测试数据 - 模拟CRAQ中的存储数据
    TestData = #eh_storage_data{
        object_type = person,
        object_id = 10,
        timestamp = 1,
        data_index = 1,
        status = ?STATUS_ACTIVE,
        column = name,
        value = john
    },
    
    io:format("1. 原始数据:~n   ~p~n~n", [TestData]),
    
    %% 2. 序列化为二进制格式
    BinaryData = entry_to_binary(TestData),
    BinarySize = byte_size(BinaryData),
    
    io:format("2. 序列化结果:~n"),
    io:format("   总长度: ~p 字节~n", [BinarySize]),
    io:format("   二进制数据前32字节: ~p~n~n", [binary:part(BinaryData, 0, min(32, BinarySize))]),
    
    %% 3. 分解二进制格式
    <<Sha:?SHA_BYTE_SIZE/binary, 
      DataSize:?DATA_BIT_SIZE, 
      SerializedData:DataSize/binary>> = BinaryData,
    
    io:format("3. 二进制格式分解:~n"),
    io:format("   SHA1哈希 (20字节): ~p~n", [Sha]),
    io:format("   数据长度 (4字节): ~p~n", [DataSize]),
    io:format("   序列化数据前20字节: ~p~n~n", [binary:part(SerializedData, 0, min(20, DataSize))]),
    
    %% 4. 展示term_to_binary的原始输出
    RawBinary = term_to_binary(TestData),
    RawBinaryList = binary_to_list(RawBinary),
    
    io:format("4. term_to_binary原始输出:~n"),
    io:format("   长度: ~p 字节~n", [byte_size(RawBinary)]),
    io:format("   前20字节: ~p~n", [lists:sublist(RawBinaryList, 20)]),
    io:format("   完整字节列表: ~p~n~n", [RawBinaryList]),
    
    %% 5. 保存到文件
    FileName = "test_storage_data.bin",
    ok = save_to_file(FileName, BinaryData),
    io:format("5. 数据已保存到文件: ~s~n~n", [FileName]),
    
    %% 6. 从文件读取
    {ok, LoadedBinary} = load_from_file(FileName),
    io:format("6. 从文件读取数据:~n"),
    io:format("   读取长度: ~p 字节~n", [byte_size(LoadedBinary)]),
    io:format("   数据匹配: ~p~n~n", [BinaryData =:= LoadedBinary]),
    
    %% 7. 反序列化验证 - 修正二进制分解
    LoadedBinarySize = byte_size(LoadedBinary),
    if 
        LoadedBinarySize >= (?SHA_BYTE_SIZE + 4) ->
            <<LoadedSha:?SHA_BYTE_SIZE/binary, 
              LoadedDataSize:?DATA_BIT_SIZE,
              LoadedSerializedData/binary>> = LoadedBinary,
            
            %% 确保有足够的数据
            if 
                byte_size(LoadedSerializedData) >= LoadedDataSize ->
                    ActualSerializedData = binary:part(LoadedSerializedData, 0, LoadedDataSize),
                    Result = binary_to_entry(<<LoadedSha:?SHA_BYTE_SIZE/binary, LoadedDataSize:?DATA_BIT_SIZE>>, 
                                            ActualSerializedData),
                    
                    io:format("7. 反序列化结果:~n"),
                    case Result of
                        {ok, DeserializedData} ->
                            io:format("   成功: ~p~n", [DeserializedData]),
                            io:format("   数据完整性: ~p~n~n", [TestData =:= DeserializedData]);
                        ?EH_BAD_DATA ->
                            io:format("   失败: 数据损坏~n~n")
                    end;
                true ->
                    io:format("7. 反序列化失败: 数据不完整~n~n")
            end;
        true ->
            io:format("7. 反序列化失败: 文件头部不完整~n~n")
    end,
    
    %% 8. 性能测试
    performance_test(TestData),
    
    %% 9. 数据损坏检测测试
    test_corruption_detection(),
    
    %% 10. 清理文件
    file:delete(FileName),
    io:format("~n10. 测试完成，临时文件已删除~n").

%% 序列化函数 - 完全模拟eh_storage_data_api:entry_to_binary/1
-spec entry_to_binary(Entry :: #eh_storage_data{}) -> binary().
entry_to_binary(Entry) ->
    %% 步骤1: Erlang原生序列化
    Bin = term_to_binary(Entry),
    BinSize = byte_size(Bin),
    
    %% 步骤2: 计算SHA1哈希
    Sha = compute_hash(BinSize, Bin),
    
    %% 步骤3: 组装最终二进制格式
    <<Sha:?SHA_BYTE_SIZE/binary,
      BinSize:?DATA_BIT_SIZE,
      Bin:BinSize/binary>>.

%% 反序列化函数 - 完全模拟eh_storage_data_api:binary_to_entry/2
-spec binary_to_entry(Bin1 :: binary(), Bin2 :: binary()) -> {ok, #eh_storage_data{}} | ?EH_BAD_DATA.
binary_to_entry(<<Sha:?SHA_BYTE_SIZE/binary, BinSize:?DATA_BIT_SIZE>>, Bin2) ->
    case compute_hash(BinSize, Bin2) of
        Sha ->
            {ok, binary_to_term(Bin2)};
        _Other ->
            ?EH_BAD_DATA
    end.

%% 哈希计算函数 - 完全模拟eh_storage_data_api:compute_hash/2
-spec compute_hash(BinSize :: non_neg_integer(), Bin :: binary()) -> binary().
compute_hash(BinSize, Bin) ->
    B1 = <<BinSize:?DATA_BIT_SIZE, Bin:BinSize/binary>>,
    crypto:hash(sha, B1).

%% 保存到文件
save_to_file(FileName, BinaryData) ->
    file:write_file(FileName, BinaryData).

%% 从文件加载
load_from_file(FileName) ->
    file:read_file(FileName).

%% 性能测试
performance_test(TestData) ->
    io:format("8. 性能测试 (1000次操作):~n"),
    
    %% 序列化性能测试
    {SerTime, _} = timer:tc(fun() ->
        [entry_to_binary(TestData) || _ <- lists:seq(1, 1000)]
    end),
    
    %% 反序列化性能测试
    BinaryData = entry_to_binary(TestData),
    <<Sha:?SHA_BYTE_SIZE/binary, 
      DataSize:?DATA_BIT_SIZE, 
      SerializedData:DataSize/binary>> = BinaryData,
    
    {DeserTime, _} = timer:tc(fun() ->
        [binary_to_entry(<<Sha:?SHA_BYTE_SIZE/binary, DataSize:?DATA_BIT_SIZE>>, 
                         SerializedData) || _ <- lists:seq(1, 1000)]
    end),
    
    %% term_to_binary性能对比
    {TermTime, _} = timer:tc(fun() ->
        [term_to_binary(TestData) || _ <- lists:seq(1, 1000)]
    end),
    
    io:format("   序列化 (含哈希): ~.2f μs/次~n", [SerTime/1000]),
    io:format("   反序列化 (含验证): ~.2f μs/次~n", [DeserTime/1000]),
    io:format("   纯term_to_binary: ~.2f μs/次~n", [TermTime/1000]),
    io:format("   哈希计算开销: ~.2f μs/次~n", [(SerTime-TermTime)/1000]).

%% 测试数据损坏检测
test_corruption_detection() ->
    io:format("~n9. 数据损坏检测测试:~n"),
    
    TestData = #eh_storage_data{
        object_type = person,
        object_id = 10,
        timestamp = 1,
        data_index = 1,
        status = ?STATUS_ACTIVE,
        column = name,
        value = john
    },
    
    BinaryData = entry_to_binary(TestData),
    <<Sha:?SHA_BYTE_SIZE/binary, 
      DataSize:?DATA_BIT_SIZE, 
      SerializedData:DataSize/binary>> = BinaryData,
    
    %% 故意损坏数据 - 修改最后一个字节
    CorruptedData = binary:replace(SerializedData, binary:part(SerializedData, DataSize-1, 1), <<255>>),
    
    Result = binary_to_entry(<<Sha:?SHA_BYTE_SIZE/binary, DataSize:?DATA_BIT_SIZE>>, 
                            CorruptedData),
    
    case Result of
        ?EH_BAD_DATA ->
            io:format("   ✓ 成功检测到数据损坏~n");
        {ok, _} ->
            io:format("   ✗ 未能检测到数据损坏~n")
    end.
