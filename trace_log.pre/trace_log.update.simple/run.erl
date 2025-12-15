
f().
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
File="trace_update_ec_n1.txt".
Path="/home/ericksun/program/erlang_craq/trace_log/".
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_data_server, eh_system_server].
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).



f().
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
File="trace_update_ec_n2.txt".
Path="/home/ericksun/program/erlang_craq/trace_log/".
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_data_server, eh_system_server].
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).



f().
N1 = 'ec_n1@centos7-dev'.
N2 = 'ec_n2@centos7-dev'.
N3 = 'ec_n3@centos7-dev'.
NodeList = [N1, N2, N3].
File="trace_update_ec_n3.txt".
Path="/home/ericksun/program/erlang_craq/trace_log/".
FileName= string:concat(Path, File).
file:delete(FileName).
ListMod=[eh_data_server, eh_system_server].
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', '_'}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 10000, [{scope, local}, {io_server, Dev},{args, arity}]).
