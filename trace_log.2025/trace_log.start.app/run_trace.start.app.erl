
f().
File="trace_log.start.app.node1.txt".
%File="trace_log.start.app.node2.txt".
%File="trace_log.start.app.node3.txt".
LogNum=50000.
Path="/home/ericksun/program/erlang_craq/trace_log.2025/trace_log.start.app/".
file:make_dir(FileName).
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_app,eh_file_name,eh_storage_data_api,eh_update_msg,eh_aq_query_handler_api,eh_node_state,eh_storage_data,eh_wait_query_handler_api,eh_config,eh_node_timestamp,eh_storage_data_operation_api,eh_write_conflict_resolver_api,eh_data_server,eh_no_wait_query_handler_api,eh_sup,eh_write_conflict_resolver,eh_data_util,eh_persist_data,eh_system_config,eh_dirty_read_query_handler_api,eh_persist_storage_data,eh_system_server,eh_event,eh_query_handler,eh_system_sup,erlang_craq,eh_event_handler,eh_repl_data_manager_api,eh_system_util,erlang_craq_validate,eh_failure_detector_api,eh_repl_data_manager,eh_unique_id_generator_api,eh_failure_detector,eh_repl_ring,eh_unique_id_generator].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).


%N1 = 'ec_n1@rabbitmq4-1'.
%N2 = 'ec_n2@rabbitmq4-2'.
%N3 = 'ec_n3@rabbitmq4-3'.
%NodeList = [N1, N2, N3].
%erlang_craq:setup_repl(NodeList).
%ListMod=[eh_aq_query_handler_api,eh_data_server,eh_dirty_read_query_handler_api,eh_event,eh_failure_detector_api,eh_failure_detector,eh_file_name,eh_node_state,eh_node_timestamp,eh_no_wait_query_handler_api,eh_persist_data,eh_persist_storage_data,eh_query_handler,eh_repl_data_manager_api,eh_repl_data_manager,eh_storage_data_api,eh_storage_data,eh_storage_data_operation_api,eh_system_server,eh_wait_query_handler_api,eh_write_conflict_resolver_api,eh_write_conflict_resolver,erlang_craq_validate].
