#!/bin/bash

#cd /home/ericksun/program/erlang_craq
#rebar3 shell --config config/sys.config --sname $1
rebar3 shell --sname $1



# 在rebar.config中 有如下信息的是时候不用加 --config
#        {sys_config, "./config/sys.config"},
#        {vm_args, "./config/vm.args"}
#
