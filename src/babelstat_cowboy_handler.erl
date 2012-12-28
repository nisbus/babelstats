%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%   A websocket handler for the babelstat service.
%%%   Takes a json document for calling for data in the form:
%%%   {"query":
%%%     {"category" : "cat",
%%%      "sub_category" : "sub",
%%%      "subject" : "subject",
%%%      "series_category" : "series_category", 
%%%      "title": "title",
%%%      "frequency":"frequency",
%%%      "scale":"scale",
%%%      "metric":"metric",
%%%      "from_date": "optional",
%%%      "to_date": "optional"}
%%%    }
%%%
%%%    This is mainly for fun and has no real use cases, use the rest 
%%%    interface instead :)
%%% @end
%%% Created : 26 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(babelstat_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
% Behaviour cowboy_http_handler  
-export([init/3, handle/2, terminate/2]).  
% Behaviour cowboy_http_websocket_handler  
-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).  

%% API
-record(state,
	{
	  callback,
	  sim_pid
	}).

% Called to know how to dispatch a new connection.  
init(_Any, _Req, _Opts) ->      
    {upgrade, protocol, cowboy_websocket}.
  
% Should never get here.  
handle(_Req, State) ->  
    {ok, Req2} = cowboy_http_req:reply(404, [  
        {'Content-Type', <<"text/html">>}  
    ]),  
    {ok, Req2, State}.  
  
terminate(_Req, _State) ->  
    ok.  
  
% Called for every new websocket connection.  
websocket_init(_Any, Req, []) ->  
    Client = self(),

    C = fun(X) -> {result,Series} = X,
		  F = babelstat_api:result_to_proplist(Series),
		  Json = jiffy:encode({F}),
		  Client ! {send,Json} end,
    {ok, Req, #state{callback = C}}.  
  
% Called when a text message arrives.  
websocket_handle({text, Msg}, Req, State) ->  
    handle_message(Msg,Req,State);
  
websocket_handle(_Any, Req, State) ->  
    
    {ok, Req, State}.  

websocket_info({send,Message}, Req, State) -> 
    {reply, {text, Message}, Req, State};
  
websocket_info(_Info, Req, State) ->     
    {ok, Req, State, hibernate}.  
  
websocket_terminate(_Reason, _Req, _State) ->  
    ok.  

handle_message(Msg,Req,#state{callback = Callback} = State) ->
    {JSON} = jiffy:decode(Msg),
    {Q} = proplists:get_value(<<"query">>,JSON),
    Category = re:replace(proplists:get_value(<<"category">>,Q),<<"\"">>,<<"">>,[global]),
    SubCategory = re:replace(proplists:get_value(<<"sub_category">>,Q),<<"\"">>,<<"">>,[global]),
    Subject = re:replace(proplists:get_value(<<"subject">>,Q),<<"\"">>,<<"">>,[global]),
    SeriesCategory = re:replace(proplists:get_value(<<"series_category">>,Q),<<"\"">>,<<"">>,[global]),
    Title = re:replace(proplists:get_value(<<"title">>,Q),<<"\"">>,<<"">>,[global]),

    Query = babelstat_api:create_query(Category, SubCategory, Subject,SeriesCategory, Title),
    Metric = re:replace(proplists:get_value(<<"metric">>,Q),<<"\"">>,<<"">>,[global]),
    Scale = re:replace(proplists:get_value(<<"scale">>,Q),<<"\"">>,<<"">>,[global]),
    Frequency = re:replace(proplists:get_value(<<"frequency">>,Q),<<"\"">>,<<"">>,[global]),
    From = proplists:get_value(<<"from_date">>,Q),
    To = proplists:get_value(<<"to_date">>,Q),
    Scale0 = list_to_integer(binary_to_list(Scale)),
    Frequency0 = list_to_atom(binary_to_list(Frequency)),
    Filter = babelstat_api:create_filter(Metric,Scale0,Frequency0,From,To),
    babelstat_api:run_query(Query, Filter,Callback),
    {ok, Req,State}.
