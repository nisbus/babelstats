%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @author omarkj <omarkj@gmail.com>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%%   The babelstat application.
%%%   Creates the dispatch rules and starts the web server.
%%% @end
%%% Created :  9 Jul 2011 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(babelstat).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [{'_',[
		  {[<<"ws">>], babelstat_cowboy_handler,[]},
		  {[<<"index.html">>,'...'],cowboy_static,
		   [ {directory,{priv_dir, babelstat,[]}},
		     {file,<<"index.html">>},
		     {mimetypes,[{<<".html">>,[<<"text/html">>]}]}]},
		  {[<<"main.css">>,'...'],cowboy_static,
		   [ {directory,{priv_dir, babelstat,[]}},
		     {file,<<"main.css">>},
		     {mimetypes,[{<<".css">>,[<<"text/css">>]}]}]},
		  {[<<"highcharts.js">>,'...'],cowboy_static,
		   [ {directory,{priv_dir, babelstat,[]}},
		     {file,<<"highcharts.js">>},
		     {mimetypes,[{<<".js">>,[<<"text/javascript">>]}]}]},
		  {[<<"loader.js">>,'...'],cowboy_static,
		   [ {directory,{priv_dir, babelstat,[]}},
		     {file,<<"loader.js">>},
		     {mimetypes,[{<<".js">>,[<<"text/javascript">>]}]}]},



		      {[<<"jquery.flot.js">>,'...'],cowboy_static,
		       [ {directory, {priv_dir, monterl_carlo_websocket,[]}},
			 {file,<<"jquery.flot.js">>},
			 {mimetypes, [{<<".js">>, [<<"text/javascript">>]}]}]},
		      {'_', babelstat_cowboy_rest_handler,[]}
		 ]}],
    cowboy:start_http(http,100,[{port, 8080}],[{dispatch, Dispatch}]),
    babelstat_sup:start_link().

stop(_State) ->
    ok.
