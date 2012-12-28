%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%   A http couchdb implementation instead of the native one.
%%% @end
%%% Created : 25 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(babelstat_couch_http).
-behavior(gen_babelstat_db).
-include("../include/babelstat.hrl").
%% API
-export([query_database/2, save_document/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec query_database(#babelstat_query{},
		     #babelstat_filter{}) ->
			    {ok, Results::[#babelstat{}]} |
			    no_results.
query_database(#babelstat_query{ category = Category,
				 sub_category = SubCategory,
				 subject = Subject,
				 series_category = SeriesCategory,
				 title = Title },
	       #babelstat_filter{} = Filter) ->
    Key = [Category,
	   SubCategory,
	   Subject,
	   SeriesCategory,
	   Title],
    query_database(Key, Filter);
query_database(Key, #babelstat_filter{ from_date = undefined,
					to_date = undefined }) ->
    StartKey = Key ++ [<<"0001-01-01 00:00:00Z">>],
    EndKey = Key ++ [<<"9999-01-01 00:00:00Z">>],
    query_database(StartKey, EndKey);
query_database(Key, #babelstat_filter{ from_date = FromDate,
				       to_date = ToDate }) when from_date =/= undefined;
								to_date =/= undefined ->
    StartKey = Key ++ [to_iso(FromDate)],
    EndKey = Key ++ [to_iso(ToDate)],
    query_database(StartKey, EndKey);

query_database(StartKey, EndKey) ->
    Url = generate_url(StartKey,EndKey),
    Rows = httpc:request(get, {Url,[]},[],[]),
    case Rows of
	{ok, {{_,200,_},_,Res}} ->
	    {J} = jiffy:decode(Res),
	    R = proplists:get_value(<<"rows">>,J),
	    Results = lists:map(fun({Row}) -> 
					  {Doc} = proplists:get_value(<<"doc">>, Row),
					  document_to_babelstat(Doc)
				  end,R),
	    {ok, Results};
	{error, Error} ->
	    {error, Error}
    end.

-spec save_document(#babelstat{} | db_result()) ->
		      ok | error.
save_document(#babelstat {} = Babelstat) ->
    save_document(babelstat_to_document(Babelstat#babelstat{created_date = erlang:localtime_to_universaltime(erlang:localtime())}));
save_document(Doc) ->   
    Json = jiffy:encode(Doc),

    Url = generate_save_url(),   
    case httpc:request(post, {Url,[],"application/json",Json},[],[]) of
	{ok, _Resp} ->
	    ok;
	E ->
	    io:format("Error saving ~p\n",[E]),
	    error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_save_url() ->
    {Base,Db,_,_} = get_config(),
    Base++"/"++Db.

generate_url(StartKey, EndKey) ->
    {Base,Db,Design,View} = get_config(),
    S0 = create_key(StartKey),
    S = http_uri:encode(S0),
    E0 = create_key(EndKey),
    E = http_uri:encode(E0),
    Base++"/"++Db++"/"++Design++"/"++View++"?startkey="++S++"&endkey="++E++"&include_docs=true".

create_key(Key) ->
    K = lists:foldl(fun(Param,Acc) ->
			    AsString = "\""++binary_to_list(Param)++"\"",
			    
			    case Acc of
				[] ->
				    Acc++AsString;
				_ -> Acc++","++AsString
			    end
		end, [],Key),
    
    "["++K++"]".

babelstat_to_document(#babelstat { constant = Constant,
				   date = Date,
				   value = Value,
				   metric = Metric,
				   scale = Scale,
				   frequency = Frequency,
				   location = Location,
				   category = Category,
				   sub_category = SubCategory,
				   subject = Subject,
				   series_category = SeriesCategory,
				   title = Title,
				   calculation = Calculation,
				   source = Source,
				   created_date = CreateDate
				 }) ->
    {[{<<"type">>, <<"babelstat">>},
      {<<"constant">>, Constant},
      {<<"date">>, to_iso(Date)},
      {<<"value">>, Value},
      {<<"metric">>, Metric},
      {<<"scale">>, Scale},
      {<<"frequency">>, Frequency},
      {<<"location">>, Location},
      {<<"category">>, Category},
      {<<"sub_category">>, SubCategory},
      {<<"subject">>, Subject},
      {<<"series_category">>, SeriesCategory},
      {<<"title">>, Title},
      {<<"calculation">>, check_undefined(Calculation)},
      {<<"source">>, Source},
      {<<"created_date">>, to_iso(CreateDate)}]}.

document_to_babelstat(Doc) ->
    #babelstat{ id = proplists:get_value(<<"_id">>, Doc),
		rev = proplists:get_value(<<"_rev">>, Doc),
		constant = proplists:get_value(<<"contstant">>, Doc, false),
		date = to_date(proplists:get_value(<<"date">>, Doc)),
		value = proplists:get_value(<<"value">>, Doc),
		metric = proplists:get_value(<<"metric">>, Doc),
		scale = proplists:get_value(<<"scale">>, Doc),
		frequency = to_atom(proplists:get_value(<<"frequency">>, Doc)),
		location = to_list(proplists:get_value(<<"location">>, Doc)),
		category = proplists:get_value(<<"category">>, Doc),
		sub_category = proplists:get_value(<<"sub_category">>, Doc),
		subject = proplists:get_value(<<"subject">>, Doc),
		series_category = proplists:get_value(<<"series_category">>, Doc),
		title = proplists:get_value(<<"title">>, Doc),
		calculation = proplists:get_value(<<"calculation">>, Doc, false),
		source = to_list(proplists:get_value(<<"source">>, Doc)),
		created_date = to_date(proplists:get_value(<<"created_date">>, Doc))
	      }.

%2011-07-10 08:59:10Z
to_date(<<Year:4/binary,_:1/binary,Month:2/binary,_:1/binary,Day:2/binary>>) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, {0,0,0}};

to_date(<<Year:4/binary,_:1/binary,Month:2/binary,_:1/binary,Day:2/binary,_:1/binary,Hour:2/binary,_:1/binary,Minute:2/binary, _:1/binary, Second:2/binary ,_/binary>>) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, {binary_to_integer(Hour),binary_to_integer(Minute),binary_to_integer(Second)}}.

binary_to_integer(B) ->
    list_to_integer(binary_to_list(B)).

to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Year0 = create_date_part(Year),
    Month0 = create_date_part(Month),
    Day0 = create_date_part(Day),
    Hour0 = create_date_part(Hour),
    Minute0 = create_date_part(Minute),
    Second0 = create_date_part(Second),
    <<Year0/binary, "-", Month0/binary, "-", Day0/binary, " ", Hour0/binary, ":", Minute0/binary, ":", Second0/binary, "Z">>.

create_date_part(Date) when is_integer(Date) ->
    create_date_part(integer_to_list(Date));
create_date_part(Date) when length(Date) =:= 4;
			    length(Date) =:= 2 ->
    list_to_binary(Date);
create_date_part(Date) when length(Date) =:= 1 ->
    list_to_binary(string:right(Date, 2, $0)).

check_undefined(undefined) ->
    null;
check_undefined(Else) ->
    Else.


to_atom(Value) when is_binary(Value) ->
    list_to_atom(string:to_lower(binary_to_list(Value))).

to_list([]) ->
    [];
to_list({Proplist}) ->
    Proplist;
to_list(List) ->
    List.

get_config() ->
    {ok, Couch} = application:get_env(babelstat, couchdb_connection),
    Base = proplists:get_value(base_url,Couch),
    DB = proplists:get_value(db_name,Couch),
    Design = proplists:get_value(design_name, Couch),
    View = proplists:get_value(view_name, Couch),
    {Base, DB, Design,View}.
