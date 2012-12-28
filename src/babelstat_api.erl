%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @author omarkj <omarkj@gmail.com>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%%   The api used internally by the web server.
%%% @end
%%% Created :  9 Jul 2011 by omarkj <omarkj@gmail.com>
%%%-------------------------------------------------------------------
-module(babelstat_api).
-include("../include/babelstat.hrl").
-export([create_filter/5,
	 create_query/5,
	 run_query/3,
	 result_to_proplist/1,
	 result_to_json/1]).

%%% @doc
%%%   Creates a babelstat_filter record from given values.
%%% @end
-spec create_filter(Metric::binary(),
		    Scale::integer(),
		    Frequency::frequency(),
		    FromDate::babel_date() | undefined,
		    ToDate::babel_date() | undefined) ->
			   #babelstat_filter{} | invalid_filter.
create_filter(Metric, Scale, Frequency, FromDate, ToDate) when is_binary(Metric),
							       is_integer(Scale),
							       is_atom(Frequency) ->
    #babelstat_filter{ metric = Metric,
		       scale = Scale,
		       frequency = Frequency,
		       from_date = FromDate,
		       to_date = ToDate}.

%%% @doc
%%%   Creates a babelstat_query record from given values.
%%% @end
-spec create_query(Category::binary(), SubCategory::binary(), Subject::binary(),
		   SeriesCategory::binary(), Title::binary()) ->
			  #babelstat_query{} | invalid_query.
create_query(Category, SubCategory, Subject, SeriesCategory, Title) ->
    #babelstat_query{ category = Category,
		      sub_category = SubCategory,
		      subject = Subject,
		      series_category = SeriesCategory,
		      title = Title}.

%%% @doc
%%%  Runs a query on the server (this is IT).
%%% @end
-spec run_query(Query::#babelstat_query{}, Filter::#babelstat_filter{},
		Callback::fun()) ->
		       {ok, Pid::pid()}.
run_query(Query, Filter, Callback) ->
    babelstat_calculation_sup:add_child(Query, Filter, Callback).

%%% @doc
%%%  Converts a babelstat_series record to a proplist
%%% @end
-spec result_to_proplist(#babelstat_series{}) ->
				[term()].
result_to_proplist(#babelstat_series{ series = Series,
				      metric = Metric,
				      scale = Scale,
				      frequency = Frequency,
				      location = Location,
				      category = Category,
				      sub_category = SubCategory,
				      subject = Subject,
				      series_category = SeriesCategory,
				      title = Title,
				      source = Source,
				      legend = Legend }) ->
    {Dates, Values} = lists:foldl(fun({Date,Value},Acc) ->
					  {Dates,Values} = Acc,
					  {Dates++[to_iso(Date)],Values++[Value]}
				  end,{[],[]},Series),
    [{dates, Dates},
     {values, Values},
     {metric, Metric},
     {scale, Scale},
     {frequency, Frequency},
     {location, Location},
     {category, Category},
     {sub_category, SubCategory},
     {subject, Subject},
     {series_category, SeriesCategory},
     {title, Title},
     {source, Source},
     {legend, Legend}];

result_to_proplist(no_results) ->
    {[{<<"result">>,<<"no_results">>}]};
result_to_proplist(_) ->
    {[{<<"result">>,<<"unknown_results">>}]}.

%%% @doc
%%%  Turns a babelstat_series into a json ready form
%%% @end
-spec result_to_json(#babelstat_series{}) ->
				[term()].
result_to_json(#babelstat_series{ series = Series,
				      metric = Metric,
				      scale = Scale,
				      frequency = Frequency,
				      location = Location,
				      category = Category,
				      sub_category = SubCategory,
				      subject = Subject,
				      series_category = SeriesCategory,
				      title = Title,
				      source = Source,
				      legend = Legend }) ->
    {Dates, Values} = lists:foldl(fun({Date,Value},Acc) ->
					  {Dates,Values} = Acc,
					  {Dates++[to_iso(Date)],Values++[Value]}
				  end,{[],[]},Series),
    [
     {[{dates, Dates}]},
     {[{values, Values}]},
     {[{metric, Metric}]},
     {[{scale, Scale}]},
     {[{frequency, Frequency}]},
     {[{location, Location}]},
     {[{category, Category}]},
     {[{sub_category, SubCategory}]},
     {[{subject, Subject}]},
     {[{series_category, SeriesCategory}]},
     {[{title, Title}]},
     {[{source, Source}]},
     {[{legend, Legend}]}
    ];

result_to_json(no_results) ->
    {[{<<"result">>,<<"no_results">>}]};
result_to_json(_) ->
    {[{<<"result">>,<<"unknown_results">>}]}.
    
    

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
