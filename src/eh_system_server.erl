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

-module(eh_system_server).

-behavior(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("erlang_craq.hrl").

-define(SERVER, ?EH_SYSTEM_SERVER).

start_link(AppConfig) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [AppConfig], []).


init([AppConfig]) ->
  State = #eh_system_state{app_config=AppConfig},
  {ok, State}.


handle_call({?EH_GET_DATA, {ObjectType, ObjectId}},
            _From,
            #eh_system_state{app_config=AppConfig}=State) ->
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  Reply = ReplDataManager:get_data({ObjectType, ObjectId}),
  {reply, Reply, State};
  
handle_call(?EH_DATA_VIEW, 
            _From, 
            #eh_system_state{app_config=AppConfig}=State) ->
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  Reply = ReplDataManager:data_view(),
  {reply, Reply, State};
  
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.



%1:52:51.047604 <0.222.0> eh_system_server:handle_cast({eh_setup_repl,['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev']}, 
%                 {eh_system_state,eh_ready,
%                 0,
%                 ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%                 ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%                 'ec_n3@centos7-dev','ec_n2@centos7-dev',#{},#{},
%                 {set,0,16,16,8,80,48,
%                      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
%                      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},
%                 #{},#{},undefined,
%                 {eh_app_config,'ec_n1@centos7-dev',sorted,
%                                eh_failure_detector_api,
%                                eh_repl_data_manager_api,eh_storage_data_api,
%                                eh_write_conflict_resolver_api,
%                                eh_unique_id_generator_api,
%                                eh_wait_query_handler_api,lager_event,10000,
%                                "./","0000000000","ec_n1_repl.data",
%                                standard_io,true,100,1,2000}})
handle_cast({?EH_SETUP_REPL, ReplRing}, 
            #eh_system_state{app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),   % 获取 node_id 比如: 'ec_n1@centos7-dev'
  NodeOrder = eh_system_config:get_node_order(AppConfig), % 返回: sorted
  {ReplRing1, ReplRingOrder1, Pred, Succ} = eh_repl_ring:get_ordered_list_pred_succ(NodeId, ReplRing, ReplRing, NodeOrder),
  FailureDetector = eh_system_config:get_failure_detector(AppConfig),
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  FailureDetector:set(NodeId, ReplRing),
  {Timestamp, _} = ReplDataManager:timestamp(),
  NewState1 = eh_node_state:update_state_ready(State),
  NewState2 = NewState1#eh_system_state{repl_ring_order=ReplRingOrder1, repl_ring=ReplRing1, predecessor=Pred, successor=Succ, timestamp=Timestamp},
  event_state("setup_repl.99", NewState2, AppConfig),
  {noreply, NewState2};

handle_cast({?EH_ADD_NODE, {Node, NodeList, NodeOrderList}}, 
            #eh_system_state{app_config=AppConfig}=State) ->
  FailureDetector = eh_system_config:get_failure_detector(AppConfig),
  NodeOrder = eh_system_config:get_node_order(AppConfig),
  NodeId = eh_system_config:get_node_id(AppConfig),
  {NodeList1, NodeOrderList1, Pred1, Succ1} = eh_repl_ring:get_ordered_list_pred_succ(NodeId, NodeList, NodeOrderList, NodeOrder),
  NewState9 = case eh_node_timestamp:valid_add_node_msg(Node, State) of
                ?EH_VALID_FOR_NEW      ->
                  NewState1 = process_snapshot_request(NodeList1, NodeOrderList1, Succ1, State),
                  FailureDetector:set(Node, NodeList1),
                  NewState2 = eh_node_state:update_state_transient(NewState1),
                  NewState2#eh_system_state{repl_ring_order=NodeOrderList1, repl_ring=NodeList1, predecessor=Pred1, successor=Succ1};
                ?EH_VALID_FOR_EXISTING ->
                  FailureDetector:set(Node),
                  State#eh_system_state{repl_ring_order=NodeOrderList1, repl_ring=NodeList1, predecessor=Pred1, successor=Succ1};
                _                      ->
                  State
              end,
  event_state("add_node.99", NewState9, AppConfig),
  {noreply, NewState9};

handle_cast({?EH_SNAPSHOT, {Node, NodeList, NodeOrderList,  Ref, {Timestamp, Snapshot}}}, 
            #eh_system_state{pre_msg_data=PreMsgData, app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),
  NodeOrder = eh_system_config:get_node_order(AppConfig),
  {NodeList1, NodeOrderList1, Pred1, Succ1} = eh_repl_ring:get_ordered_list_pred_succ(NodeId, NodeList, NodeOrderList, NodeOrder),
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  Q0 = ReplDataManager:snapshot(Timestamp, Snapshot),
  PendingPreMsgMap = eh_update_msg:filter_effective_head_node_id(PreMsgData, State),
  gen_server:cast({?EH_SYSTEM_SERVER, Node}, {?EH_UPDATE_SNAPSHOT, {Ref, Q0, PendingPreMsgMap}}),
  NewState2 = State#eh_system_state{repl_ring_order=NodeOrderList1, repl_ring=NodeList1, predecessor=Pred1, successor=Succ1},
  event_state("snapshot.99", NewState2, AppConfig),
  {noreply, NewState2};

handle_cast({?EH_UPDATE_SNAPSHOT, {Ref, Q0, PendingPreMsgData}}, 
            #eh_system_state{snapshot_ref=Ref, app_config=AppConfig}=State) ->
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  ReplDataManager:update_snapshot(Q0),
  PendingPreMsgData1 = case eh_node_timestamp:valid_pending_pre_msg_data(PendingPreMsgData, State) of
                         true  ->
                           PendingPreMsgData;
                         false ->
                           eh_system_util:new_map()
                       end,
  NewState2 = eh_node_state:update_state_snapshot(State),
  NewState3 = NewState2#eh_system_state{pending_pre_msg_data=PendingPreMsgData1},
  event_state("update_snapshot.99", NewState3, AppConfig),
  {noreply, NewState3};

handle_cast({?EH_UPDATE_SNAPSHOT, _}, State) ->
  {noreply, State};

handle_cast({?EH_QUERY, {From, Ref, {ObjectType, ObjectId}}}, 
            #eh_system_state{pre_msg_data=PreMsgData, app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),
  State1 = case eh_node_state:data_state(State) of
             ?EH_NOT_READY ->
               eh_query_handler:reply(From, Ref, eh_query_handler:error_node_unavailable(NodeId)),
               State;
             _             ->  
               case eh_update_msg:exist_map_msg(ObjectType, ObjectId, PreMsgData) of
                 undefined ->
                    eh_query_handler:query(ok, ObjectType, ObjectId, From, Ref, State);
                 MsgNodeId -> 
                    QueryHandler = eh_system_config:get_query_handler(AppConfig),
                    QueryHandler:process(ObjectType, ObjectId, MsgNodeId, From, Ref, State)
               end
           end,
  {noreply, State1};

handle_cast({?EH_QUERY_AQ, {ObjectType, ObjectId, From, Ref}},
            #eh_system_state{predecessor=Pred, app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),
  State1 = case eh_node_state:data_state(State) of
             ?EH_NOT_READY ->
               case Pred of
                 undefined ->
                   eh_query_handler:reply(From, Ref, eh_query_handler:error_node_unavailable(NodeId)),
                   State;
                 Other     ->
                   eh_query_handler:process_tail(ObjectType, ObjectId, Other, From, Ref, State)
               end;
             _             ->
               eh_query_handler:query(ok, ObjectType, ObjectId, From, Ref, State)
           end,
  {noreply, State1};

handle_cast({?EH_UPDATE, {From, Ref, ObjectList}},
            #eh_system_state{timestamp=Timestamp, 
			     successor=Succ, 
			     completed_set=CompletedSet, 
			     app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),
  NewState9 = case eh_node_state:client_state(State) of
                ?EH_NOT_READY ->
                  eh_query_handler:reply(From, Ref, eh_query_handler:error_node_unavailable(NodeId)),
                  State;
                _             ->
                  Timestamp1 = Timestamp+1,
                  {NodeId, UpdateList} = lists:keyfind(NodeId, 1, ObjectList),
                  UMsgList = eh_update_msg:get_msg(UpdateList, 
                                                   Timestamp1,
                                                   From,
                                                   NodeId,
                                                   Ref),
                  NewState1 = eh_node_timestamp:update_state_timestamp(Timestamp1, State),
                case Succ of 
                    undefined ->
                      reply_to_client(fun eh_persist_data:persist_data/2, UMsgList, CompletedSet, NewState1);
                    _         -> 
                      send_pre_update_msg(fun eh_persist_data:no_persist_data/2, UMsgList, CompletedSet, NewState1)
                  end                            
              end,
  event_state("update.99", NewState9, AppConfig),
  {noreply, NewState9};

handle_cast({?EH_PRED_PRE_UPDATE, {UMsgList, CompletedSet}}, State) ->
  NewState1 = process_msg(?EH_PRED_PRE_UPDATE,
                          fun eh_node_timestamp:valid_pre_update_msg/2,   % 判断是否应该继续往前传播
                          fun send_update_msg/4,                          % 尾部节点，如果不需要往前传播, 则需要发送 send_update_msg
                          fun eh_persist_data:persist_data/2,
                          fun send_pre_update_msg/4,                      % 中间节点，如果需要往前传播，则继续传播
                          fun eh_persist_data:no_persist_data/2,
                          UMsgList,
                          CompletedSet,
                          State),
  {noreply, NewState1};% 正向确认
handle_cast({?EH_SUCC_UPDATE, {UMsgList, CompletedSet}}, State) ->
  NewState1 = process_msg(?EH_SUCC_UPDATE,
                          fun eh_node_timestamp:valid_update_msg/2,      % 判断是否应该更新信息, 逆时针往前传播
                          fun reply_to_client/4,                         % 头部节点 如果不需要继续逆时针往前传播，返回给客户端 reply_to_client 
                          fun eh_persist_data:persist_data/2,
                          fun send_update_msg/4,                         % 中间节点，如果继续往前传播，则继续传播
                          fun eh_persist_data:persist_data/2,            % 只有来自 EH_SUCC_UPDATE 的方向，更新之后才会持久化
                          UMsgList,
                          CompletedSet,
                          State),
  {noreply, NewState1};

handle_cast({stop, Reason}, #eh_system_state{app_config=AppConfig}=State) ->
  event_data("stop", status, stopped, AppConfig),
  {stop, Reason, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(Msg, 
            #eh_system_state{repl_ring=ReplRing, 
			     repl_ring_order=ReplRingOrder, 
			     predecessor=Pred, 
			     successor=Succ, 
			     pre_msg_data=PreMsgData, 
			     msg_data=MsgData, 
			     app_config=AppConfig}=State) ->
  FailureDetector = eh_system_config:get_failure_detector(AppConfig),
  NewState9 = case FailureDetector:detect(Msg) of
                {?EH_NODEDOWN, DownNode} ->
                  event_data("failure", node_down, eh_system_util:get_node_name(DownNode), AppConfig),
                  NodeId = eh_system_config:get_node_id(AppConfig),
                  NodeOrder = eh_system_config:get_node_order(AppConfig),
                  {NewReplRing, _} = eh_repl_ring:drop(DownNode, ReplRing, ReplRingOrder, NodeOrder),
                  NewPred = eh_repl_ring:predecessor(NodeId, NewReplRing, ReplRingOrder, NodeOrder),
                  NewSucc = eh_repl_ring:successor(NodeId, NewReplRing, ReplRingOrder, NodeOrder),
                  NewState1 = State#eh_system_state{repl_ring=NewReplRing, predecessor=NewPred, successor=NewSucc},
                  NewState3 = case {NewSucc, eh_node_state:snapshot_state(NewState1), Succ =:= DownNode, Pred =:= DownNode} of
                                {undefined, ?EH_READY, _, _} ->
                                  process_down_msg(NewState1);
                                {_, ?EH_NOT_READY, true, _} ->
                                  process_snapshot_request(NewReplRing, ReplRingOrder, NewSucc, NewState1);
                                {_, ?EH_READY, true, false} ->
                                  send_down_msg(?EH_PRED_PRE_UPDATE,
                                                fun eh_persist_data:no_persist_data/2,
                                                fun send_pre_update_msg/4,
                                                fun eh_persist_data:persist_data/2,
                                                fun send_update_msg/4,
                                                PreMsgData,
                                                NewState1);
                                {_, ?EH_READY, false, true} ->
                                  send_down_msg(?EH_SUCC_UPDATE,
                                                fun eh_persist_data:no_persist_data/2,
                                                fun send_update_msg/4,
						fun eh_persist_data:no_persist_data/2,
					        fun reply_to_client/4,
                                                MsgData,
                                                NewState1);
                                {_, _, _, _} ->
                                  NewState1  
                              end,
                  event_state("failure.99", NewState3, AppConfig),
                  NewState3;
                _                        ->
                  State 
              end,
   {noreply, NewState9}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

event_state(Msg, State, AppConfig) ->
  eh_event:state(?MODULE, Msg, State, AppConfig).

event_message(Msg, MsgList, CompletedSet, AppConfig) ->
  eh_event:message(?MODULE, Msg, {MsgList, CompletedSet}, AppConfig).

event_data(Msg, DataMsg, Data, AppConfig) ->
  eh_event:data(?MODULE, Msg, {DataMsg, Data}, AppConfig).

reply_to_client(PersistFun,
                UMsgList,
                _CompletedSet,
                State) -> 
  State1 = PersistFun(UMsgList, State),   % 持久化数据
  {_, ClientId, _, Ref, DataList} = eh_update_msg:get_data_list(UMsgList),
  % 持久化数据之后，返回客户端
  eh_query_handler:reply(ClientId, Ref, eh_query_handler:updated(DataList)),
  eh_node_timestamp:update_state_client_reply(UMsgList, State1).

% 2:11:31.473152 <0.222.0> eh_system_server:send_msg(eh_pred_pre_update, fun eh_persist_data:no_persist_data/2, [{{eh_update_msg_key,3,candidate,10},
send_msg(Tag, 
         PersistFun,
         UMsgList,
         CompletedSet,
         #eh_system_state{predecessor=Pred, successor=Succ}=State) ->
  State1 = PersistFun(UMsgList, State),   %% 在这里持久化数据
  %2:11:31.474708 <0.222.0> eh_node_timestamp:update_state_new_msg(eh_pred_pre_update, [{{eh_update_msg_key,3,candidate,10},
  State2 = eh_node_timestamp:update_state_new_msg(Tag, UMsgList, State1),
  Dest = case Tag of
           ?EH_PRED_PRE_UPDATE ->
             Succ;
           ?EH_SUCC_UPDATE     ->
             Pred
         end,
  % {Name :: atom(), Node :: atom()} 
  % Dest 就是要发往的下一个节点, 上面持久化数据之后就发往下一个节点
  gen_server:cast({?EH_SYSTEM_SERVER, Dest}, {Tag, {UMsgList, CompletedSet}}),
  State2.  

% 正向确认
send_pre_update_msg(PersistFun,
                    UMsgList,
                    CompletedSet,
                    State) ->
  send_msg(?EH_PRED_PRE_UPDATE, PersistFun, UMsgList, CompletedSet, State).
 
% 更新成功之后，反向确认
send_update_msg(PersistFun,
                UMsgList,
                CompletedSet,
		State) ->
  send_msg(?EH_SUCC_UPDATE, PersistFun, UMsgList, CompletedSet, State).

%% 节点3
%4:30:54.398282 <0.222.0> eh_node_timestamp:valid_pre_update_msg/2 --> {true,eh_tail_msg,
%    {eh_system_state,eh_ready,4,
%        ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%        ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%        'ec_n2@centos7-dev','ec_n1@centos7-dev',#{},
%
%4:30:54.399267 <0.222.0>  '--> eh_system_server:process_msg/9
%
%
%% 对比节点2
%3:50:15.776078 <0.302.0> eh_node_timestamp:valid_pre_update_msg/2 --> {true,eh_ring_msg,
%    {eh_system_state,eh_ready,3,
%        ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%        ['ec_n1@centos7-dev','ec_n2@centos7-dev','ec_n3@centos7-dev'],
%        'ec_n1@centos7-dev','ec_n3@centos7-dev',#{},
process_msg(Tag,
            ValidateMsgFun,
            ReturnedMsgFun,
            ReturnedMsgPersistFun,
            ValidMsgFun,
            ValidMsgPersistFun,
            UMsgList,
            CompletedSet,
            #eh_system_state{app_config=AppConfig}=State) ->
  DisplayTag = eh_system_util:display_atom_to_list(Tag),
  {MsgTimestamp, _, _, _} = eh_update_msg:get_msg_param(UMsgList),
  NewState8 = case eh_node_state:msg_state(State) of
                ?EH_NOT_READY ->
                  event_message(DisplayTag++".invalid_msg", UMsgList, CompletedSet, AppConfig),
                  State;
                _             ->
                  case ValidateMsgFun(UMsgList, State) of
                    {false, _, NewState1}           ->
                      event_message(DisplayTag++".duplicate_msg", UMsgList, CompletedSet, AppConfig),
                      NewState1;
                    {true, ?EH_HEAD_MSG, NewState1} ->
                      event_message(DisplayTag++".valid_head_msg", UMsgList, CompletedSet, AppConfig),
                      NewState2 = eh_node_timestamp:update_state_timestamp(MsgTimestamp, NewState1),
                      NewState3 = eh_node_timestamp:update_state_completed_set(CompletedSet, NewState2),
                      ReturnedMsgFun(ReturnedMsgPersistFun, UMsgList, CompletedSet, NewState3);
                    {true, ?EH_TAIL_MSG, NewState1} ->
                      event_message(DisplayTag++".valid_tail_msg", UMsgList, CompletedSet, AppConfig),
                      NewState2 = eh_node_timestamp:update_state_timestamp(MsgTimestamp, NewState1),
                      NewState3 = eh_node_timestamp:update_state_msg_data(CompletedSet, NewState2),
                      ReturnedMsgFun(ReturnedMsgPersistFun, UMsgList, CompletedSet, NewState3);
                    {true, _, NewState1}            ->
                      event_message(DisplayTag++".valid_ring_msg", UMsgList, CompletedSet, AppConfig),
                      NewState2 = eh_node_timestamp:update_state_timestamp(MsgTimestamp, NewState1),
                      NewState3 = eh_node_timestamp:update_state_msg_data(CompletedSet, NewState2),
                      ValidMsgFun(ValidMsgPersistFun, UMsgList, CompletedSet, NewState3)
                  end
              end,
  event_state(DisplayTag++".99", NewState8, AppConfig),
  NewState8.

process_snapshot_request(NodeList, NodeOrderList, Succ, #eh_system_state{app_config=AppConfig}=State) ->
  NodeId = eh_system_config:get_node_id(AppConfig),
  UniqueIdGenerator = eh_system_config:get_unique_id_generator(AppConfig),
  ReplDataManager = eh_system_config:get_repl_data_manager(AppConfig),
  SnapshotRef = UniqueIdGenerator:unique_id(),
  {Timestamp, Snapshot} = ReplDataManager:timestamp(),
  gen_server:cast({?EH_SYSTEM_SERVER, Succ}, {?EH_SNAPSHOT, {NodeId, NodeList, NodeOrderList, SnapshotRef, {Timestamp, Snapshot}}}),
  State#eh_system_state{snapshot_ref=SnapshotRef}.

process_down_msg(PersistFun,
                 MsgMap,
                 State) ->
  MsgList = eh_update_msg:get_map_msg_list(MsgMap),
  lists:foldl(fun({_, UMsgList}, StateX) -> reply_to_client(PersistFun, UMsgList, undefined, StateX) end, State, MsgList).

process_down_msg(#eh_system_state{pre_msg_data=PreMsgData, msg_data=MsgData}=State) ->
  State1 = process_down_msg(fun eh_persist_data:persist_data/2, PreMsgData, State),
  State2 = process_down_msg(fun eh_persist_data:no_persist_data/2, MsgData, State1),
  State2#eh_system_state{pre_msg_data=eh_system_util:new_map(), msg_data=eh_system_util:new_map(), completed_set=eh_system_util:new_set()}.
  
send_down_msg(Tag,
              PersistRingFun,
              RingFun,
              PersistReturnFun,
              ReturnFun,
              UMsgList,
              CompletedSet,
              State) ->
  case {Tag, eh_node_timestamp:msg_status(UMsgList, State)} of
    {?EH_PRED_PRE_UPDATE, ?EH_TAIL_MSG} ->
      ReturnFun(PersistReturnFun, UMsgList, CompletedSet, State);
    {?EH_SUCC_UPDATE, ?EH_HEAD_MSG}     ->
      ReturnFun(PersistReturnFun, UMsgList, CompletedSet, State);
    {_, _}                              ->
      RingFun(PersistRingFun, UMsgList, CompletedSet, State)
  end.

send_down_msg(Tag,
              PersistRingFun,
              RingFun,
              PersistReturnFun,
              ReturnFun,
              MsgMap,
              State) ->
  CompletedSet = eh_system_util:new_set(),
  MsgList = eh_update_msg:get_map_msg_list(MsgMap),
  lists:foldl(fun({_, UMsgList}, StateX) -> send_down_msg(Tag, PersistRingFun, RingFun, PersistReturnFun, ReturnFun, UMsgList, CompletedSet, StateX) end, State, MsgList).






                 




