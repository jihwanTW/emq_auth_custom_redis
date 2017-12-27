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

-module(emq_acl).

-include_lib("emqttd/include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init(Opts) ->
    {ok, Opts}.

%% client 쪽에서 publish 못하게끔 . subscribe 는 언제나 client 쪽에서만 가능하도록 ?
%%
%% allow // deny
check_acl({Client, PubSub, Topic}, _Opts) ->
    Client_id = Client#mqtt_client.client_id,
    Username = Client#mqtt_client.username,
    io:format("ACL Demo 2 : >~p ~n>~p ~n>~p~n", [Client, PubSub, Topic]),
    io:format("~p ~p ~n", [Client_id, Username]),
    Result = case Client_id of
                 <<"server">>->
                     allow;
                 _->
                     {Success_result,Pid} = eredis:start_link(),
                     Result1 = case Success_result of
                                   ok->
                                       {ok,Redis_result} = eredis:q(Pid,["GET", Client_id]),
                                       case Redis_result of
                                           undefined->
                                               deny;%{error,<<"session key is not undefined">>};
                                           ok->
                                               allow;
                                           _->
                                               io:format("redis result : [~p]~n",[Redis_result]),
                                               allow
                                       end
                                   ;
                                   _->
                                       deny%{error,<<"eredis start link error">>}
                               end,
                     exit(Pid,normal),
                     Result1
             end,
    Result.

reload_acl(_Opts) ->
    ok.

description() -> "ACL Demo Module".
 
