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

-module(emq_auth_custom_redis_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_custom_redis_sup:start_link(),
    ok = emqttd_access_control:register_mod(auth, emq_auth, []),
    ok = emqttd_access_control:register_mod(acl, emq_acl, []),
    emq_auth_custom_redis:load(application:get_all_env()),

    % start query server  -> check acl or auth
    % cus_redis -> using redis db
    permission_server:start_link(cus_redis),

    % start emysql
    application:start(emysql),
    emysql:add_pool(
        db,
        [{size,1},
            {user,"root"},
            {password,"jhkim1020"},
            {database,"with_taehyun_project_sub"},
            {encoding,utf8}
        ]),
    {ok, Sup}.

stop(_State) ->
    ok = emqttd_access_control:unregister_mod(auth, emq_auth),
    ok = emqttd_access_control:unregister_mod(acl, emq_acl),
    emq_auth_custom_redis:unload().
