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

-module(eh_system_config).

-export([get_env/0, 
         get_node_id/1,
         get_node_order/1,
         get_failure_detector/1,
         get_repl_data_manager/1,
         get_storage_data/1,
         get_write_conflict_resolver/1,
         get_unique_id_generator/1,
         get_query_handler/1,
	 get_event_logger/1,
	 get_data_checkpoint/1,
	 get_data_dir/1,
         get_file_repl_data/1,
	 get_file_repl_data_suffix/1,
         get_file_repl_log/1,
         get_debug_mode/1,
         get_sup_restart_intensity/1,
         get_sup_restart_period/1,
         get_sup_child_shutdown/1]).

-include("erlang_craq.hrl").

-define(NODE_ORDER,                ?EH_SORTED).
-define(FAILURE_DETECTOR,          eh_failure_detector_api).
-define(REPL_DATA_MANAGER,         eh_repl_data_manager_api).
-define(STORAGE_DATA,              eh_storage_data_api).
-define(WRITE_CONFLICT_RESOLVER,   eh_write_conflict_resolver_api).
-define(UNIQUE_ID_GENERATOR,       eh_unique_id_generator_api).
-define(QUERY_HANDLER,             eh_no_wait_query_handler_api).
-define(EVENT_LOGGER,              ?LAGER_EVENT).
-define(DATA_CHECKPOINT,           10000).
-define(DATA_DIR,                  "./").
-define(FILE_REPL_DATA,            "_repl.data").
-define(FILE_REPL_DATA_SUFFIX,     "0000000000").
-define(FILE_REPL_LOG,             standard_io).
-define(DEBUG_MODE,                false).
-define(SUP_RESTART_INTENSITY,     1).
-define(SUP_RESTART_PERIOD,        5).
-define(SUP_CHILD_SHUTDOWN,        2000).

%3:19:08.096900 <0.311.0> eh_system_config:get_env/0 --> {eh_app_config,'ec_n1@centos7-dev',sorted,eh_failure_detector_api,
%               eh_repl_data_manager_api,eh_storage_data_api,
%               eh_write_conflict_resolver_api,eh_unique_id_generator_api,
%               eh_wait_query_handler_api,lager_event,10000,"./","0000000000",
%               "ec_n1_repl.data",standard_io,true,100,1,2000}
-spec get_env() -> #eh_app_config{}.
get_env() ->
  Node         = node(),
  NodeName     = eh_system_util:get_node_name(Node),
  DataDir      = eh_config:get_env(erlang_craq, data_dir, ?DATA_DIR),
  FileReplData = eh_config:get_env(erlang_craq, file_repl_data, ?FILE_REPL_DATA),
  FileReplLog  = eh_config:get_env(erlang_craq, file_repl_log, ?FILE_REPL_LOG),

  #eh_app_config{node_id                  = Node,
                 node_order               = eh_config:get_env(erlang_craq, node_order,               ?NODE_ORDER),
                 failure_detector         = eh_config:get_env(erlang_craq, failure_detector,         ?FAILURE_DETECTOR),
                 repl_data_manager        = eh_config:get_env(erlang_craq, repl_data_manager,        ?REPL_DATA_MANAGER),
                 storage_data             = eh_config:get_env(erlang_craq, storage_data,             ?STORAGE_DATA),
                 write_conflict_resolver  = eh_config:get_env(erlang_craq, write_conflict_resolver,  ?WRITE_CONFLICT_RESOLVER),
                 unique_id_generator      = eh_config:get_env(erlang_craq, unique_id_generator,      ?UNIQUE_ID_GENERATOR),
                 query_handler            = eh_config:get_env(erlang_craq, query_handler,            ?QUERY_HANDLER),
                 event_logger             = eh_config:get_env(erlang_craq, event_logger,             ?EVENT_LOGGER),
                 data_checkpoint          = eh_config:get_env(erlang_craq, data_checkpoint,          ?DATA_CHECKPOINT),
                 data_dir                 = DataDir,
                 file_repl_data           = eh_file_name:get_file_name(NodeName, FileReplData),
                 file_repl_data_suffix    = eh_config:get_env(erlang_craq, file_repl_data_suffix,    ?FILE_REPL_DATA_SUFFIX),
                 file_repl_log            = eh_file_name:get_full_file_name(DataDir, eh_file_name:get_file_name(NodeName, FileReplLog)),
                 debug_mode               = eh_config:get_env(erlang_craq, debug_mode,               ?DEBUG_MODE),
                 sup_restart_intensity    = eh_config:get_env(erlang_craq, sup_restart_intensity,    ?SUP_RESTART_INTENSITY),
                 sup_restart_period       = eh_config:get_env(erlang_craq, sup_restart_period,       ?SUP_RESTART_PERIOD),
                 sup_child_shutdown       = eh_config:get_env(erlang_craq, sup_child_shutdown,       ?SUP_CHILD_SHUTDOWN)}.

-spec get_node_id(AppConfig :: #eh_app_config{}) -> atom().
get_node_id(#eh_app_config{node_id=NodeId}) ->
  NodeId.

-spec get_node_order(AppConfig :: #eh_app_config{}) -> atom().
get_node_order(#eh_app_config{node_order=NodeOrder}) ->
  NodeOrder.

-spec get_failure_detector(AppConfig :: #eh_app_config{}) -> atom().
get_failure_detector(#eh_app_config{failure_detector=FailureDetector}) ->
  FailureDetector.

-spec get_repl_data_manager(AppConfig :: #eh_app_config{}) -> atom().
get_repl_data_manager(#eh_app_config{repl_data_manager=ReplDataManager}) ->
  ReplDataManager.

-spec get_storage_data(AppConfig :: #eh_app_config{}) -> atom().
get_storage_data(#eh_app_config{storage_data=StorageData}) ->
  StorageData.

-spec get_write_conflict_resolver(AppConfig :: #eh_app_config{}) -> atom().
get_write_conflict_resolver(#eh_app_config{write_conflict_resolver=WriteConflictResolver}) ->
  WriteConflictResolver.

-spec get_unique_id_generator(AppConfig :: #eh_app_config{}) -> atom().
get_unique_id_generator(#eh_app_config{unique_id_generator=UniqueIdGenerator}) ->
  UniqueIdGenerator.

-spec get_query_handler(AppConfig :: #eh_app_config{}) -> atom().
get_query_handler(#eh_app_config{query_handler=QueryHandler}) ->
  QueryHandler.

-spec get_event_logger(AppConfig :: #eh_app_config{}) -> atom().
get_event_logger(#eh_app_config{event_logger=EventLogger}) ->
  EventLogger.

-spec get_data_checkpoint(AppConfig :: #eh_app_config{}) -> non_neg_integer().
get_data_checkpoint(#eh_app_config{data_checkpoint=DataCheckPoint}) ->
    DataCheckPoint.

-spec get_data_dir(AppConfig :: #eh_app_config{}) -> string().
get_data_dir(#eh_app_config{data_dir=DataDir}) ->
    DataDir.

-spec get_file_repl_data(AppConfig :: #eh_app_config{}) -> string().
get_file_repl_data(#eh_app_config{file_repl_data=FileReplData}) ->
  FileReplData.

-spec get_file_repl_data_suffix(AppConfig :: #eh_app_config{}) -> string().
get_file_repl_data_suffix(#eh_app_config{file_repl_data_suffix=FileReplDataSuffix}) ->
    FileReplDataSuffix.

-spec get_file_repl_log(AppConfig :: #eh_app_config{}) -> atom() | string().
get_file_repl_log(#eh_app_config{file_repl_log=FileReplLog}) ->
  FileReplLog.

-spec get_debug_mode(AppConfig :: #eh_app_config{}) -> boolean().
get_debug_mode(#eh_app_config{debug_mode=DebugMode}) ->
  DebugMode.

-spec get_sup_restart_intensity(AppConfig :: #eh_app_config{}) -> non_neg_integer().
get_sup_restart_intensity(#eh_app_config{sup_restart_intensity=SupRestartIntensity}) ->
  SupRestartIntensity.

-spec get_sup_restart_period(AppConfig :: #eh_app_config{}) -> non_neg_integer().
get_sup_restart_period(#eh_app_config{sup_restart_period=SupRestartPeriod}) ->
  SupRestartPeriod.

-spec get_sup_child_shutdown(AppConfig :: #eh_app_config{}) -> non_neg_integer().
get_sup_child_shutdown(#eh_app_config{sup_child_shutdown=SupChildShutdown}) ->
  SupChildShutdown.






