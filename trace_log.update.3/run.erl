
f().
File="trace_update_ec_n1.txt".
Path="/home/ericksun/program/erlang_craq/trace_log.update.3/".
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
file:make_dir(FileName).
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_aq_query_handler_api,eh_data_server,eh_dirty_read_query_handler_api,eh_event,eh_failure_detector_api,eh_failure_detector,eh_file_name,eh_node_state,eh_node_timestamp,eh_no_wait_query_handler_api,eh_persist_data,eh_persist_storage_data,eh_query_handler,eh_repl_data_manager_api,eh_repl_data_manager,eh_storage_data_api,eh_storage_data,eh_storage_data_operation_api,eh_system_server,eh_wait_query_handler_api,eh_write_conflict_resolver_api,eh_write_conflict_resolver,erlang_craq_validate].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).

f().
File="trace_update_ec_n2.txt".
Path="/home/ericksun/program/erlang_craq/trace_log.update.3/".
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
file:make_dir(FileName).
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_aq_query_handler_api,eh_data_server,eh_dirty_read_query_handler_api,eh_event,eh_failure_detector_api,eh_failure_detector,eh_file_name,eh_node_state,eh_node_timestamp,eh_no_wait_query_handler_api,eh_persist_data,eh_persist_storage_data,eh_query_handler,eh_repl_data_manager_api,eh_repl_data_manager,eh_storage_data_api,eh_storage_data,eh_storage_data_operation_api,eh_system_server,eh_wait_query_handler_api,eh_write_conflict_resolver_api,eh_write_conflict_resolver,erlang_craq_validate].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).


f().
File="trace_update_ec_n3.txt".
Path="/home/ericksun/program/erlang_craq/trace_log.update.3/".
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
file:make_dir(FileName).
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_aq_query_handler_api,eh_data_server,eh_dirty_read_query_handler_api,eh_event,eh_failure_detector_api,eh_failure_detector,eh_file_name,eh_node_state,eh_node_timestamp,eh_no_wait_query_handler_api,eh_persist_data,eh_persist_storage_data,eh_query_handler,eh_repl_data_manager_api,eh_repl_data_manager,eh_storage_data_api,eh_storage_data,eh_storage_data_operation_api,eh_system_server,eh_wait_query_handler_api,eh_write_conflict_resolver_api,eh_write_conflict_resolver,erlang_craq_validate].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).


