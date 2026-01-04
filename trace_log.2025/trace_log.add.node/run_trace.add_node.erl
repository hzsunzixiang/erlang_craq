
f().
File="trace_log.add_node.node1.txt".
%File="trace_log.add_node.node2.txt".
%File="trace_log.add_node.node3.txt".

%% 加载所有的模块,这里加载的
LP = fun() -> [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] end.
LP().  %% 同步加载

LogNum=50000.

Path="/home/ericksun/program/erlang_craq/trace_log.2025/trace_log.add.node/".
file:make_dir(FileName).
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_repl_ring,eh_app,eh_file_name,eh_storage_data_api,eh_update_msg,eh_aq_query_handler_api,eh_node_state,eh_storage_data,eh_wait_query_handler_api,eh_config,eh_node_timestamp,eh_storage_data_operation_api,eh_write_conflict_resolver_api,eh_data_server,eh_no_wait_query_handler_api,eh_sup,eh_write_conflict_resolver,eh_data_util,eh_persist_data,eh_system_config,eh_dirty_read_query_handler_api,eh_persist_storage_data,eh_system_server,eh_event,eh_query_handler,eh_system_sup,erlang_craq,eh_event_handler,eh_repl_data_manager_api,eh_system_util,erlang_craq_validate,eh_failure_detector_api,eh_repl_data_manager,eh_failure_detector,eh_unique_id_generator_api,eh_unique_id_generator].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).
