N1 = 'ec_n1@rabbitmq4-1'.
N2 = 'ec_n2@rabbitmq4-2'.
N3 = 'ec_n3@rabbitmq4-3'.
NodeList = [N1, N2, N3].
%% 假设 N1 已经在运行，要添加 N2 和 N3
%% 步骤1：添加 N2（N2 会从 N1 同步数据，因为 N1 是 N2 的后继）
erlang_craq:add_node(N2, [N1, N2], [N1, N2, N3]).
%% 步骤2：等待 N2 同步完成后，添加 N3（N3 会从 N2 同步数据）
erlang_craq:add_node(N3, [N1, N2, N3], [N1, N2, N3]).

%erlang_craq:setup_repl(NodeList).
