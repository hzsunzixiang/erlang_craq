N1 = 'ec_n1@rabbitmq4-1'.
erlang_craq:update(N1, person, 10, [{name, john}, {age, 30}, {gender, male}]).
erlang_craq:update(N1, person, 10, [{age, 40}]).
erlang_craq:update(N1, person, 10, [{name, john_smith}], [age]).
erlang_craq:update(N1, [{candidate, 10, [{name,donald_trump},{party,republican}]}, {person, 10, [{name,john_smith},{age,40}]}]).
