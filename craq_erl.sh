#!/bin/bash

#cd /home/ericksun/program/erlang_craq
rebar3 shell --config config/sys.config --sname $1
