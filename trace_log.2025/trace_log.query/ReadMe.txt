
从节点N1发起
N1 = 'ec_n1@rabbitmq4-1'.
N2 = 'ec_n2@rabbitmq4-2'.
N3 = 'ec_n3@rabbitmq4-3'.
NodeList = [N1, N2, N3].
erlang_craq:query(N1, person, 10).
