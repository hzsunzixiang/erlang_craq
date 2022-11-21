%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2016 Gyanendra Aggarwal.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eh_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%% 3:19:08.096509 <0.310.0> eh_sup:start_link()
-spec start_link() -> {ok, pid()} | term().
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% 3:19:08.096581 <0.311.0> eh_sup:init([])
-spec init(Arg :: list()) -> {ok, {tuple(), list()}}.
init([]) ->
  AppConfig        = eh_system_config:get_env(),
  RestartIntensity = eh_system_config:get_sup_restart_intensity(AppConfig),
  RestartPeriod    = eh_system_config:get_sup_restart_period(AppConfig),
  ChildShutdown    = eh_system_config:get_sup_child_shutdown(AppConfig),

  Sup   = {eh_system_sup, {eh_system_sup, start_link, []},
           permanent, ChildShutdown, supervisor, []},
  Event = {eh_event,      {eh_event,      start_link, []},
           permanent, ChildShutdown, worker, dynamic},
   
  Childern        = [Sup, Event],
  RestartStrategy = {one_for_one, RestartIntensity, RestartPeriod},

  {ok, {RestartStrategy, Childern}}.

%3:19:08.098791 <0.311.0> eh_sup:init/1 --> {ok,{{one_for_one,100,1},
%     [{eh_system_sup,{eh_system_sup,start_link,[]},
%                     permanent,2000,supervisor,[]},
%      {eh_event,{eh_event,start_link,[]},permanent,2000,worker,dynamic}]}}

%3:19:08.096900 <0.311.0> eh_system_config:get_env/0 --> {eh_app_config,'ec_n1@centos7-dev',sorted,eh_failure_detector_api,
%               eh_repl_data_manager_api,eh_storage_data_api,
%               eh_write_conflict_resolver_api,eh_unique_id_generator_api,
%               eh_wait_query_handler_api,lager_event,10000,"./","0000000000",
%               "ec_n1_repl.data",standard_io,true,100,1,2000}



