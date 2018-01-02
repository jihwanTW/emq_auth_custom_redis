%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_auth_custom_redis).

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/4, on_client_unsubscribe/4]).

-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).

-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

-record(result_packet, {seq_num, field_list, rows, extra}).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
    emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
    emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
    emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
    emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]),
    emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
    emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

on_client_connected(ConnAck, Client = #mqtt_client{client_id = Client_id,client_pid = Client_pid}, _Env) ->
    io:format("client2 ~s connected, connack: ~w // pid : ~p~n", [Client_id, ConnAck,pid_to_list(self())]),

    case Client_id of
        <<"server">>->
            pass;
        _->
            %% 타입에따라 달라지게끔 수정.
            Pools = get_personal_sub_topics(Client_id),
            emqttd_client:subscribe(Client_pid, get_common_sub_topic(0)++Pools)
    end,

    {ok, Client}.

on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client2 ~s disconnected, reason: ~w~n", [ClientId, Reason]),
    ok.

%% return 으로 ok 만 넘어가도 정상적으로 작동은함. 대신 Qos 어떻게 넘어가는지 모름.
%% dafd 반환 -> 무엇으로 넘어가든지 상관없이 정상작동
on_client_subscribe(ClientId, Username, TopicTable, _Env) ->
    io:format("client2(~s/~s) will subscribe: ~p~n", [Username, ClientId, TopicTable]),
    {ok, TopicTable}
.

on_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
    io:format("client2(~s/~s) unsubscribe ~p~n", [ClientId, Username, TopicTable]),
    {ok, TopicTable}
.


on_session_created(ClientId, Username, _Env) ->
    io:format("session2(~s/~s) created.", [ClientId, Username]).

%% return 으로 무엇이 넘어가든 상관없이 정상작동
on_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session2(~s/~s) subscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    {ok, {Topic, Opts}}
.

on_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session2(~s/~s) unsubscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    ok.

on_session_terminated(ClientId, Username, Reason, _Env) ->
    io:format("session2(~s/~s) terminated: ~p.", [ClientId, Username, Reason]).

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("publish ~s~n", [emqttd_message:format(Message)]),
    {ok, Message}.

on_message_delivered(ClientId, Username, Message, _Env) ->
    io:format("delivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

on_message_acked(ClientId, Username, Message, _Env) ->
    io:format("client(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
    emqttd:unhook('session.created', fun ?MODULE:on_session_created/3),
    emqttd:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
    emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
    emqttd:unhook('session.terminated', fun ?MODULE:on_session_terminated/4),
    emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
    emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4).



%internal Functions
get_common_sub_topic(Selected)->
    case Selected of
        0->
            [
                {<<"board">>,0},
                {<<"notice">>,0},
                {<<"update">>,0}
            ];
        _->
            error
    end

    .

get_personal_sub_topics(Client_id)->
    User_idx = permission_server:get_user_idx(Client_id),
    Sql = "SELECT group_concat(subscribe) as all_subscribes FROM subscribes WHERE user_idx = ?",
    Result = query_execute(db,sub,Sql,[User_idx]),
    case Result#result_packet.rows of
        []->
            [];
        _->
            [New_result] = emysql_util:as_json(Result),
            Pools = proplists:get_value(<<"all_subscribes">>,New_result,<<"">>),
            case Pools of
                <<"">> ->
                    [];
                _->
                    List = binary:split(Pools,<<",">>,[global]),
                    lists:map(fun(X)->{X,0} end,List)
            end
    end

    .



query_execute(Db,Pool,Sql,Param)->
    emysql:prepare(Pool,Sql),
    emysql:execute(Db,Pool,Param)
.