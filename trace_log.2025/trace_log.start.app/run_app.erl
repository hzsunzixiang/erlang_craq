
craq_erl.sh ec_n11


erlang_craq:start().

[ericksun@rabbitmq4-1:~/program/erlang_craq] (master *%)$ ./craq_erl.sh ec_n1
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlang_craq
Attempting to start epmd...
Erlang/OTP 26 [erts-14.2.5.1] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

Eshell V14.2.5.1 (press Ctrl+G to abort, type help(). for help)
WARNING: This is a deprecated console configuration. Please use "[{level,info}]" instead.
===> Booted syntax_tools
===> Booted compiler
===> Booted goldrush
===> Booted lager
===> Booted recon
===> Booted erlang_craq
===> Booted sasl
(ec_n1@rabbitmq4-1)1> application:which_applications().
[{sasl,"SASL  CXC 138 11","4.2.1"},
 {erlang_craq,"chain replication apportioned query high throughput atomic store",
              "0.1.0"},
 {recon,"Diagnostic tools for production use","2.5.5"},
 {lager,"Erlang logging framework","3.9.2"},
 {goldrush,"Erlang event stream processor","0.1.9"},
 {compiler,"ERTS  CXC 138 10","8.4.3"},
 {syntax_tools,"Syntax tools","3.1"},
 {inets,"INETS  CXC 138 49","9.1"},
 {ssl,"Erlang/OTP SSL application","11.1.4.1"},
 {public_key,"Public key infrastructure","1.15.1.1"},
 {asn1,"The Erlang ASN1 compiler version 5.2.2","5.2.2"},
 {crypto,"CRYPTO","5.4.2"},
 {stdlib,"ERTS  CXC 138 10","5.2.3"},
 {kernel,"ERTS  CXC 138 10","9.2.4.1"}]
%% 这里已经启动了，不用重复启动
(ec_n1@rabbitmq4-1)2> erlang_craq:start().
{error,{already_started,erlang_craq}}


%%%%%%%%%%%%%%%%%%%%%%

[ericksun@rabbitmq4-2:~/program/erlang_craq] (master %)$ ./craq_erl.sh ec_n2
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlang_craq
Attempting to start epmd...
Erlang/OTP 26 [erts-14.2.5.1] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

Eshell V14.2.5.1 (press Ctrl+G to abort, type help(). for help)
WARNING: This is a deprecated console configuration. Please use "[{level,info}]" instead.
===> Booted syntax_tools
===> Booted compiler
===> Booted goldrush
===> Booted lager
===> Booted recon
===> Booted erlang_craq
===> Booted sasl
(ec_n2@rabbitmq4-2)1> nodes().
[]
(ec_n2@rabbitmq4-2)2>

%%%%%%%%%%%%%%%%%%%%%%

[ericksun@rabbitmq4-3:~/program/erlang_craq] (master *%)$ ./craq_erl.sh ec_n3
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlang_craq
Attempting to start epmd...
Erlang/OTP 26 [erts-14.2.5.1] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

Eshell V14.2.5.1 (press Ctrl+G to abort, type help(). for help)
WARNING: This is a deprecated console configuration. Please use "[{level,info}]" instead.
===> Booted syntax_tools
===> Booted compiler
===> Booted goldrush
===> Booted lager
===> Booted recon
===> Booted erlang_craq
===> Booted sasl



N1 = 'ec_n1@rabbitmq4-1'.
N2 = 'ec_n2@rabbitmq4-2'.
N3 = 'ec_n3@rabbitmq4-3'.
NodeList = [N1, N2, N3].
