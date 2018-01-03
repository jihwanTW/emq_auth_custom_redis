%%%-------------------------------------------------------------------
%%% @author Twinny-KJH
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 1월 2018 오후 8:36
%%%-------------------------------------------------------------------
-module(cus_topic).
-author("Twinny-KJH").

%% API
-export([get_sub_topics/2]).

-record(result_packet, {seq_num, field_list, rows, extra}).



-spec(get_sub_topics(integer(),binary())->list()).
get_sub_topics(Selected,Client_id)->
  case Client_id of
    <<"server">>->
      [];
    _->
      get_common_sub_topics(Selected)++get_personal_sub_topics(Client_id)
  end
  .


%internal Functions
get_common_sub_topics(Selected)->
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