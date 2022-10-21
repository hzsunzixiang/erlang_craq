#!/bin/bash

#cd /home/ericksun/program/erlang_craq
erl -pa ./ebin -pa ./craq_test/ebin -pa ./deps/lager/ebin -pa ./deps/goldrush/ebin -config config/sys.config -sname $1 

