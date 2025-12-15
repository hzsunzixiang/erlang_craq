-module(file_open_demo).

-export([demo_all/0, 
         demo_basic_open/0,
         demo_open_modes/0,
         demo_performance_comparison/0,
         demo_error_handling/0,
         demo_file_descriptor_analysis/0]).

-include_lib("kernel/include/file.hrl").

%% 完整演示所有功能
demo_all() ->
    io:format("=== Erlang file:open Demo ===~n~n"),
    demo_basic_open(),
    demo_open_modes(),
    demo_performance_comparison(),
    demo_error_handling(),
    demo_file_descriptor_analysis(),
    cleanup_demo_files().

%% 基础文件打开演示
demo_basic_open() ->
    io:format("1. Basic File Open Demo~n"),
    io:format("========================~n"),
    
    FileName = "demo_basic.txt",
    
    %% 基础写入模式
    {ok, WriteFile} = file:open(FileName, [write]),
    io:format("Write mode file descriptor: ~p~n", [WriteFile]),
    
    %% 写入一些数据
    file:write(WriteFile, "Hello, Erlang file:open!~n"),
    file:write(WriteFile, "This is a demo file.~n"),
    file:close(WriteFile),
    
    %% 基础读取模式
    {ok, ReadFile} = file:open(FileName, [read]),
    io:format("Read mode file descriptor: ~p~n", [ReadFile]),
    
    %% 读取数据
    {ok, Data} = file:read(ReadFile, 1024),
    io:format("Read data: ~p~n", [Data]),
    file:close(ReadFile),
    
    io:format("~n").

%% 不同打开模式演示
demo_open_modes() ->
    io:format("2. Different Open Modes Demo~n"),
    io:format("============================~n"),
    
    FileName = "demo_modes.bin",
    
    %% CRAQ风格：append + read + binary + raw
    io:format("CRAQ Style: [append, read, binary, raw]~n"),
    {ok, CraqFile} = file:open(FileName, [append, read, binary, raw]),
    io:format("  File descriptor: ~p~n", [CraqFile]),
    
    %% 写入二进制数据
    BinaryData = term_to_binary({demo_data, 123, "test"}),
    file:write(CraqFile, BinaryData),
    io:format("  Written binary data size: ~p bytes~n", [byte_size(BinaryData)]),
    file:close(CraqFile),
    
    %% 高性能模式：delayed_write + read_ahead
    io:format("High Performance: [raw, write, delayed_write, read_ahead, binary]~n"),
    {ok, PerfFile} = file:open("demo_perf.bin", [raw, write, delayed_write, read_ahead, binary]),
    io:format("  File descriptor: ~p~n", [PerfFile]),
    file:close(PerfFile),
    
    %% 只读模式
    io:format("Read Only: [read, binary]~n"),
    {ok, ReadOnlyFile} = file:open(FileName, [read, binary]),
    io:format("  File descriptor: ~p~n", [ReadOnlyFile]),
    {ok, ReadData} = file:read(ReadOnlyFile, byte_size(BinaryData)),
    RestoredData = binary_to_term(ReadData),
    io:format("  Read and restored data: ~p~n", [RestoredData]),
    file:close(ReadOnlyFile),
    
    io:format("~n").

%% 性能对比演示
demo_performance_comparison() ->
    io:format("3. Performance Comparison Demo~n"),
    io:format("==============================~n"),
    
    DataSize = 1000,  % 减少数据量以便演示
    
    %% 慢速写入（无缓存）
    io:format("Slow write (no caching):~n"),
    {Time1, _} = timer:tc(fun() -> create_file_slow("demo_slow.bin", DataSize) end),
    io:format("  Time: ~p microseconds (~.2f ms)~n", [Time1, Time1/1000]),
    
    %% 快速写入（有缓存）
    io:format("Fast write (with delayed_write):~n"),
    {Time2, _} = timer:tc(fun() -> create_file_fast("demo_fast.bin", DataSize) end),
    io:format("  Time: ~p microseconds (~.2f ms)~n", [Time2, Time2/1000]),
    
    %% 批量写入（最优）
    io:format("Batch write (optimal):~n"),
    {Time3, _} = timer:tc(fun() -> create_file_batch("demo_batch.bin", DataSize) end),
    io:format("  Time: ~p microseconds (~.2f ms)~n", [Time3, Time3/1000]),
    
    Improvement1 = case Time2 of 0 -> 0; _ -> Time1/Time2 end,
    Improvement2 = case Time3 of 0 -> 0; _ -> Time1/Time3 end,
    io:format("  Performance improvement: ~.2fx faster (delayed_write vs no cache)~n", [Improvement1]),
    io:format("  Performance improvement: ~.2fx faster (batch vs no cache)~n", [Improvement2]),
    
    io:format("~n").

%% 错误处理演示
demo_error_handling() ->
    io:format("4. Error Handling Demo~n"),
    io:format("======================~n"),
    
    %% 尝试打开不存在的文件（只读模式）
    case file:open("nonexistent_file.txt", [read]) of
        {ok, File1} ->
            io:format("  Unexpected success: ~p~n", [File1]),
            file:close(File1);
        {error, Reason1} ->
            io:format("  Expected error opening nonexistent file: ~p~n", [Reason1])
    end,
    
    %% 尝试打开受保护的路径
    case file:open("/root/protected_file.txt", [write]) of
        {ok, File2} ->
            io:format("  Unexpected success: ~p~n", [File2]),
            file:close(File2);
        {error, Reason2} ->
            io:format("  Expected error accessing protected path: ~p~n", [Reason2])
    end,
    
    %% 正常打开然后分析
    {ok, NormalFile} = file:open("demo_normal.txt", [write, read]),
    io:format("  Normal file opened successfully: ~p~n", [NormalFile]),
    file:close(NormalFile),
    
    io:format("~n").

%% 文件描述符结构分析
demo_file_descriptor_analysis() ->
    io:format("5. File Descriptor Structure Analysis~n"),
    io:format("=====================================~n"),
    
    %% 创建不同类型的文件描述符
    {ok, StandardFile} = file:open("demo_standard.txt", [write, read]),
    {ok, RawFile} = file:open("demo_raw.bin", [write, read, binary, raw]),
    {ok, BufferedFile} = file:open("demo_buffered.bin", [write, read, binary, delayed_write, read_ahead]),
    
    io:format("Standard mode file descriptor:~n  ~p~n", [StandardFile]),
    io:format("Raw mode file descriptor:~n  ~p~n", [RawFile]),
    io:format("Buffered mode file descriptor:~n  ~p~n", [BufferedFile]),
    
    %% 分析文件描述符结构
    analyze_file_descriptor("Standard", StandardFile),
    analyze_file_descriptor("Raw", RawFile),
    analyze_file_descriptor("Buffered", BufferedFile),
    
    %% 关闭文件
    file:close(StandardFile),
    file:close(RawFile),
    file:close(BufferedFile),
    
    io:format("~n").

%% 分析文件描述符结构的辅助函数
analyze_file_descriptor(Type, #file_descriptor{module = Module, data = Data}) ->
    io:format("~s file descriptor analysis:~n", [Type]),
    io:format("  Module: ~p~n", [Module]),
    case Data of
        #{handle := Handle, owner := Owner} ->
            io:format("  Handle: ~p~n", [Handle]),
            io:format("  Owner: ~p~n", [Owner]);
        _ ->
            io:format("  Data: ~p~n", [Data])
    end.

%% 慢速文件创建（类似文档示例）
create_file_slow(Name, Size) ->
    {ok, Fd} = file:open(Name, [raw, write, binary]),
    create_file_slow_loop(Fd, Size),
    file:close(Fd).

create_file_slow_loop(_Fd, 0) ->
    ok;
create_file_slow_loop(Fd, N) ->
    ok = file:write(Fd, <<0>>),
    create_file_slow_loop(Fd, N - 1).

%% 快速文件创建（使用delayed_write）
create_file_fast(Name, Size) ->
    {ok, Fd} = file:open(Name, [raw, write, delayed_write, binary]),
    create_file_fast_loop(Fd, Size),
    file:close(Fd).

create_file_fast_loop(_Fd, 0) ->
    ok;
create_file_fast_loop(Fd, N) ->
    ok = file:write(Fd, <<0>>),
    create_file_fast_loop(Fd, N - 1).

%% 批量文件创建（最优性能）
create_file_batch(Name, Size) ->
    {ok, Fd} = file:open(Name, [raw, write, delayed_write, binary]),
    Data = binary:copy(<<0>>, Size),
    ok = file:write(Fd, Data),
    file:close(Fd).

%% 清理演示文件
cleanup_demo_files() ->
    Files = ["demo_basic.txt", "demo_modes.bin", "demo_perf.bin", 
             "demo_slow.bin", "demo_fast.bin", "demo_batch.bin",
             "demo_normal.txt", "demo_standard.txt", "demo_raw.bin", 
             "demo_buffered.bin"],
    lists:foreach(fun(F) -> file:delete(F) end, Files),
    io:format("Demo files cleaned up.~n").