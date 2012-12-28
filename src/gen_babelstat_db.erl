%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @author omarkj <omarkj@gmail.com>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%%   The behaviour for a database connection.
%%% @end
%%% @TODO
%%%   Add the methods of the view functions in couch db to the behaviour
%%% @end
%%% Created :  9 Jul 2011 by omarkj <omarkj@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_babelstat_db).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{query_database, 2},
     {save_document, 1}];
behaviour_info(_) ->
    undefined.
