N1 = 'ec_n1@rabbitmq4-1'.
N2 = 'ec_n2@rabbitmq4-2'.
N3 = 'ec_n3@rabbitmq4-3'.
NodeList = [N1, N2, N3].
erlang_craq:query(N1, person, 10).
%rlang_craq:setup_repl(NodeList).
%erlang_craq:update(N1, person, 10, [{name, john}, {age, 30}, {gender, male}]).
%erlang_craq:update(N2, person, 10, [{age, 40}]).
%erlang_craq:update(N3, person, 10, [{name, john_smith}], [age]).
%erlang_craq:update(N1, [{candidate, 10, [{name,donald_trump},{party.republican}]}, {person, 10, [{name,john_smith},{age,40}]}]).




