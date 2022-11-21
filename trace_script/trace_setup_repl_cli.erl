%%%%%%%%%%%%%%% 重定向到文件中
f().
File="trace_setup_repl_cli.txt".
Path="/home/ericksun/program/erlang_craq/trace_log/".
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_app,eh_aq_query_handler_api,eh_config,eh_data_server,eh_data_util,eh_dirty_read_query_handler_api,eh_event,eh_event_handler,eh_failure_detector_api,eh_failure_detector,eh_file_name,eh_node_state,eh_node_timestamp,eh_no_wait_query_handler_api,eh_persist_data,eh_persist_storage_data,eh_query_handler,eh_repl_data_manager_api,eh_repl_data_manager,eh_repl_ring,eh_storage_data_api,eh_storage_data,eh_storage_data_operation_api,eh_sup,eh_system_config,eh_system_server,eh_system_sup,eh_system_util,eh_unique_id_generator_api,eh_unique_id_generator,eh_update_msg,eh_wait_query_handler_api,eh_write_conflict_resolver_api,eh_write_conflict_resolver,erlang_craq,erlang_craq_validate].


DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [return_to, {scope, local}, {io_server, Dev}]).

N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
erlang_craq:setup_repl(NodeList).
