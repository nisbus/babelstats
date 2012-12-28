%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, nisbus
%%% @doc
%%%   Restful interface for the babelstat api.
%%%   You query the database using the query and filter ex.
%%  ``
%%  http://localhost/babelstat?category="category"&sub_category="sub_category"&
%%   subject="subject"&series_category="series_category"&title="title"&
%%   frequency="frequency"&metric="metric"&scale="scale"
%%  ''
%%%
%%   ``You can add the optional &from_date="iso formatted date"&to_date="iso formatted date"''
%%%
%%%   The response will tell you if any of the parameters are missing.  
%%% @end
%%% Created : 27 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(babelstat_cowboy_rest_handler).
-behaviour(cowboy_http_handler).
%% API
-export([init/3, handle/2,terminate/2]).
-include("../include/babelstat.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%%%@hidden
init({tcp,http},Req,_Opts) ->
    {ok, Req,undefined_state}.

%%%@hidden
handle(Req,State) -> 
    case get_query_and_filter(Req) of
	{ok, Query, Filter} ->
	    Pid = self(),
	    babelstat_api:run_query(Query,
				    Filter,
				    fun(Res) ->
					    Pid ! Res 
				    end),
	    receive
		{result,Res} ->
		    
		    Prop = babelstat_api:result_to_json(Res),
		    io:format("Returning ~p~n",[Prop]),
		    R = jiffy:encode(Prop),
		    {ok, Req2} = cowboy_req:reply(200,[],R,Req),
		    Req2,
		    {ok, Req2,State}						  
	    after 10000 ->
		    {ok, Req2} = cowboy_req:reply(200,[],<<"Timed out waiting for response">>,Req),
		    Req2,
		    {ok, Req2,State}						  
	    end;		
	{error, E} ->
	    io:format("Returning error ~p~n",[E]),
	    JSON = jiffy:encode(E),
	    {ok, Req2} = cowboy_req:reply(200,[],JSON,Req),
	    {ok, Req2,State};
	_ ->    
	    {ok, Req2} = cowboy_req:reply(200,[],<<"BabelStat running">>,Req),
	    Req2,
	    {ok, Req2,State}
    end.

%%%@hidden
terminate(_Req,_State) ->
    ok.

%%%@hidden
get_query_and_filter(Req) ->
    {Category,_} = cowboy_req:qs_val(<<"category">>,Req),
    {SubCategory,_} = cowboy_req:qs_val(<<"sub_category">>,Req),
    {Subject,_} = cowboy_req:qs_val(<<"subject">>,Req),
    {SeriesCategory,_} = cowboy_req:qs_val(<<"series_category">>,Req),
    {Series,_} = cowboy_req:qs_val(<<"series">>,Req),
    {Frequency,_} = cowboy_req:qs_val(<<"frequency">>,Req),
    {Metric,_} = cowboy_req:qs_val(<<"metric">>,Req),
    {Scale,_} = cowboy_req:qs_val(<<"scale">>,Req),
    {From,_} = cowboy_req:qs_val(<<"from_date">>,Req),
    {To,_} = cowboy_req:qs_val(<<"to_date">>,Req),

    ValidQuery = validate_query(Category,SubCategory,Subject,SeriesCategory,Series),
    ValidFilters = validate_filter(Metric,Scale,Frequency),
    case {ValidQuery,ValidFilters} of
	{ok,ok} ->
	    [[],Cat,[]] = re:replace(Category,<<"\"">>,<<"">>,[global]),
	    [[],SCat,[]] = re:replace(SubCategory,<<"\"">>,<<"">>,[global]),
	    [[],Sub,[]] = re:replace(Subject,<<"\"">>,<<"">>,[global]),
	    [[],SerCat,[]] = re:replace(SeriesCategory,<<"\"">>,<<"">>,[global]),
	    [[],Ser,[]] = re:replace(Series,<<"\"">>,<<"">>,[global]),
	    [[],Frq,[]] = re:replace(Frequency,<<"\"">>,<<"">>,[global]),
	    [[],Met,[]] = re:replace(Metric,<<"\"">>,<<"">>,[global]),
	    [[],Sca,[]] = re:replace(Scale,<<"\"">>,<<"">>,[global]),
	    [[],F,[]] = case From of
			      undefined -> [[],undefined,[]];
			      _ ->
				  re:replace(From,<<"\"">>,<<"">>,[global])
			  end,
	    [[],T,[]] = case To of
			      undefined -> [[],undefined,[]];
			      _ -> re:replace(To,<<"\"">>,<<"">>,[global])
			  end,
	    
	    Scale0 = list_to_integer(binary_to_list(Sca)),
	    Frequency0 = list_to_atom(binary_to_list(Frq)),
	    Query = babelstat_api:create_query(Cat,SCat,Sub,SerCat,Ser),
	    Filter = babelstat_api:create_filter(Met,Scale0,Frequency0,F,T),
	    {ok, Query,Filter};
	{{error,QE},ok} ->
	    {error, {[{<<"error in query">>,QE}]}};
	{ok,{error,FE}} ->
	    {error, {[{<<"error in filters">>,FE}]}};
	{{error,QE0},{error,FE0}} ->
	    {error, {[{<<"error in query and filters">>,lists:merge([QE0,FE0])}]}}
    end.

validate_query(C,SC,Sub,SeriesC,Series) ->
    L = [{<<"category">>,C},{<<"series_category">>,SC},{<<"sub_category">>,Sub},{<<"series_category">>,SeriesC},{<<"series">>,Series}],
    Invalid = lists:any(fun({_N,V}) ->
				io:format("Query validator ~p, ~p~n",[_N,V]),
				V =:= undefined
			end,L),
    case Invalid of
	true ->
	    {error, create_error_response(L)};
	false ->
	    ok
    end.

validate_filter(Metric,Scale,Frequency) ->
    L = [{<<"metric">>,Metric},{<<"scale">>,Scale},{<<"frequency">>,Frequency}],
    Invalid = lists:any(fun({_N,V}) ->
				io:format("Filter validator ~p, ~p~n",[_N,V]),			      
				V =:= undefined
			end,L),
    case Invalid of
	true ->
	    {error,create_error_response(L)};
	false ->
	    ok
    end.

create_error_response(L) ->
    lists:map(fun({N,V}) ->
		      case V of 
			  undefined ->
			      {[{N, missing}]};
			  _ ->
			      {[{N, valid}]}
		      end
	      end, L).
