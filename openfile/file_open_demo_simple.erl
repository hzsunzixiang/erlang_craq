-module(file_open_demo_simple).

-export([demo/0]).

-include_lib("kernel/include/file.hrl").

demo() ->
    io:format("=== Erlang file:open Demo ===~n~n"),
    
    %% 1. 基础文件打开演示
    io:format("1. Basic File Open Demo~n"),
    io:format("========================~n"),
    
    %% CRAQ风格打开
    FileName = "craq_demo.bin",
    {ok, CraqFile} = file:open(FileName, [append, read, binary, raw]),
    io:format("CRAQ style file descriptor:~n~p~n~n", [CraqFile]),
    
    %% 写入数据
    TestData = term_to_binary({eh_storage_data, person, 10, 1, 1, 1, name, john}),
    file:write(CraqFile, TestData),
    io:format("Written data size: ~p bytes~n", [byte_size(TestData)]),
    file:close(CraqFile),
    
    %% 2. 不同模式对比
    io:format("~n2. Different Open Modes~n"),
    io:format("========================~n"),
    
    %% 标准模式
    {ok, StandardFile} = file:open("standard.txt", [write, read]),
    io:format("Standard mode: ~p~n", [StandardFile]),
    file:close(StandardFile),
    
    %% Raw模式
    {ok, RawFile} = file:open("raw.bin", [write, read, binary, raw]),
    io:format("Raw mode: ~p~n", [RawFile]),
    file:close(RawFile),
    
    %% 高性能模式
    {ok, PerfFile} = file:open("perf.bin", [write, binary, raw, delayed_write]),
    io:format("Performance mode: ~p~n", [PerfFile]),
    file:close(PerfFile),
    
    %% 3. 性能测试
    io:format("~n3. Performance Test~n"),
    io:format("===================~n"),
    
    Size = 1000,
    
    %% 慢速写入
    {Time1, _} = timer:tc(fun() -> 
        {ok, F1} = file:open("slow.bin", [write, binary, raw]),
        write_loop(F1, Size),
        file:close(F1)
    end),
    
    %% 快速写入
    {Time2, _} = timer:tc(fun() -> 
        {ok, F2} = file:open("fast.bin", [write, binary, raw, delayed_write]),
        write_loop(F2, Size),
        file:close(F2)
    end),
    
    io:format("Slow write: ~p μs (~.2f ms)~n", [Time1, Time1/1000]),
    io:format("Fast write: ~p μs (~.2f ms)~n", [Time2, Time2/1000]),
    
    Improvement = case Time2 of 0 -> 0; _ -> Time1/Time2 end,
    io:format("Performance improvement: ~.2fx~n", [Improvement]),
    
    %% 4. 错误处理
    io:format("~n4. Error Handling~n"),
    io:format("==================~n"),
    
    case file:open("nonexistent.txt", [read]) of
        {ok, _} -> io:format("Unexpected success~n");
        {error, Reason} -> io:format("Expected error: ~p~n", [Reason])
    end,
    
    %% 5. 文件描述符分析
    io:format("~n5. File Descriptor Analysis~n"),
    io:format("============================~n"),
    
    {ok, AnalysisFile} = file:open("analysis.bin", [append, read, binary, raw]),
    
    case AnalysisFile of
        #file_descriptor{module = Module, data = Data} ->
            io:format("File descriptor structure:~n"),
            io:format("  Module: ~p~n", [Module]),
            io:format("  Data: ~p~n", [Data]),
            case Data of
                #{handle := Handle, owner := Owner, r_buffer := RBuffer, r_ahead_size := RAheadSize} ->
                    io:format("  Handle: ~p~n", [Handle]),
                    io:format("  Owner: ~p~n", [Owner]),
                    io:format("  Read buffer: ~p~n", [RBuffer]),
                    io:format("  Read ahead size: ~p~n", [RAheadSize]);
                _ ->
                    ok
            end;
        Other ->
            io:format("Non-record file descriptor: ~p~n", [Other])
    end,
    
    file:close(AnalysisFile),
    
    %% 清理文件
    Files = ["craq_demo.bin", "standard.txt", "raw.bin", "perf.bin", 
             "slow.bin", "fast.bin", "analysis.bin"],
    lists:foreach(fun file:delete/1, Files),
    
    io:format("~nDemo completed!~n").

%% 辅助函数：循环写入
write_loop(_File, 0) ->
    ok;
write_loop(File, N) ->
    file:write(File, <<0>>),
    write_loop(File, N-1).