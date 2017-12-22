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

-module(emq_auth).

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

init(Opts) -> {ok, Opts}.

check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
    io:format("Auth Demo 2 : clientId=~p, username=~p, password=~p~n",
              [ClientId, Username, Password]),
%%  {Success_result,Pid} = eredis:start_link(),
%%  Result = case Success_result of
%%    ok->
%%      {ok,Redis_result} = eredis:q(Pid,["GET",ClientId]),
%%      case Redis_result of
%%        undefined->
%%          error;
%%        ok->
%%          ok;
%%        _->
%%          error
%%      end
%%      ;
%%    _->
%%      error
%%  end,
%%  io:format("result is ~p ~n",[Result]),
%%  Result
ok.

description() -> "Auth Demo Module".

