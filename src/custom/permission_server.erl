%%%-------------------------------------------------------------------
%%% @author Twinny-KJH
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2018 오후 3:53
%%%-------------------------------------------------------------------
-module(permission_server).
-author("Twinny-KJH").

%% API
-export([is_user/1, get_user_idx/1]).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {api}).

start_link(Api) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Api], []).

init([Api]) ->
  Api:init_store(),
  {ok, #state{api = Api}}.

handle_call({is_user,Session_key}, _From, State = #state{api = Api}) ->
  Reply = Api:is_user(Session_key),
  {reply, Reply, State};
handle_call({get_user_idx,Session_key}, _From, State = #state{api = Api}) ->
  Reply = Api:get_user_idx(Session_key),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions


%% Export functions
%% 유저 존재여부 조회
-spec(is_user(Session_key::binary()) -> ok | undefined).
is_user(Session_key)-> gen_server:call(?MODULE,{is_user,Session_key}).

%% 유저 idx 조회 ?
-spec(get_user_idx(Session_key::binary()) -> integer() | undefined).
get_user_idx(Session_key)-> gen_server:call(?MODULE,{get_user_idx,Session_key}).

