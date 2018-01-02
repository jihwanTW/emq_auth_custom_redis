%%%-------------------------------------------------------------------
%%% @author Twinny-KJH
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2018 오후 4:10
%%%-------------------------------------------------------------------
-module(cus_redis).
-author("Twinny-KJH").

%% API
-export([is_user/1, init_store/0]).


-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {pid}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok,Pid} = eredis:start_link(),
  {ok, #state{pid = Pid}}.


%% export functions
%% 해당 gen_server 를 시작시키는 function
init_store()->
  ?MODULE:start_link().

%% session_key를 활용하여 해당 유저의 session_key 값이 등록되어있는지를 조회
is_user(Session_key) ->
  gen_server:call(?MODULE,{is_user,Session_key}).


handle_call({is_user,Session_key}, _From, State = #state{pid=Pid}) ->
  {ok,Redis_result} = eredis:q(Pid,["GET",Session_key]),
  Result = case Redis_result of
    undefined->
      undefined;
    _->
      ok
  end,
  {reply, Result, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
