%@author("TanmayDutta").
-module(dateUtils).
-compile(export_all).
% DayCountConvList carries name and the factor to multiply directly
% Its a property list with {Name,Factor} format
getFunctionInfo(FunctionName) ->
 apply(?MODULE,FunctionName,[information]).

% Erlang Quirk to store everything as integer "." is 46 and 0-9 is 48-57
parseTenor(information) ->
 io:format("Parse any tenor imaginable [currently unsafe]of form NNmm/NNm/NNddd/NNyyyy etc returns a tuple with first element as NN and second as the string token yy/dd etc etc Removes any float value. ~n");
parseTenor(TenorString) ->
 LowerTenorString = string:to_lower(TenorString),
 parseTenorH(LowerTenorString,[],[]).

parseTenorH([],Numeric,Token) -> {element(1,string:to_integer(Numeric)),Token};

parseTenorH([S|Str],Numeric,Token) ->
 if (S < 58) and (S>45) ->
 NumericNew = Numeric++[S],
 parseTenorH(Str,NumericNew,Token);
 true ->
 TokenN = Token++[S],
 parseTenorH(Str,Numeric,TokenN)
 end.
% to-do enhancement is pass the final results to a function like adjustTenorToBDCAndCountry -> (Tenor,BDC,CountryCalendar)
% this would check for the returned tenor to be a holiday or not if holiday then some follows the business conventions
adjustTenorToBDC(Tenor,BDC) ->
 {{Y,M,D},LBDC}= {Tenor,list_to_atom(string:to_lower(BDC))},
 case LBDC of
 no_change -> Tenor;
 following -> DayNum = calendar:day_of_the_week(Tenor),
 case DayNum of
 7 -> {Y,M,D+1};
 6 -> {Y,M,D+2};
 _N -> Tenor
 end;
 preceding -> DayNum = calendar:day_of_the_week(Tenor),
 case DayNum of
 7 -> {Y,M,D-2};
 6 -> {Y,M,D-1};
 _N -> Tenor
 end;
 modified_following -> DayNum = calendar:day_of_the_week(Tenor),
 case DayNum of
 7 ->
 {Yn,Mn,Dn} = addDayToTenor(Tenor,1),
 if Mn > M ->
 addDayToTenor(Tenor,-2);
 true -> {Yn,Mn,Dn}
 end;
 6 ->
 {Yn,Mn,Dn} = addDayToTenor(Tenor,2),
 if Mn > M ->
 addDayToTenor(Tenor,-1);
 true -> {Yn,Mn,Dn}
 end;
 _N -> Tenor
 end;
 modified_preceding -> DayNum = calendar:day_of_the_week(Tenor),
 case DayNum of
 7 ->
 {Yn,Mn,Dn} = addDayToTenor(Tenor,-2),
 if Mn < M ->
 addDayToTenor(Tenor,1);
 true -> {Yn,Mn,Dn}
 end;
 6 ->
 {Yn,Mn,Dn} = addDayToTenor(Tenor,-1),
 if Mn < M ->
 addDayToTenor(Tenor,2);
 true -> {Yn,Mn,Dn}
 end;
 _N -> Tenor
 end;
 nearest ->
 DayNum = calendar:day_of_the_week(Tenor),
 case DayNum of
 7 -> addDayToTenor(Tenor,1);
 6 -> addDayToTenor(Tenor,-1);
 _N -> Tenor
 end;

true -> io:format("Unknown convention ~n")
 end.

addDayToTenor(Tenor,Days) ->
 GregDays = calendar:date_to_gregorian_days(Tenor),
 NewGregDays = GregDays + Days,
 calendar:gregorian_days_to_date(NewGregDays).

create_range({Fy,Fm,Fs},{Ty,Tm,Ts},Frequency,IncludeWeekends) ->
 DateTimeRange = create_range({{Fy,Fm,Fs},{0,0,0}},{{Ty,Tm,Ts},{0,0,0}},Frequency,IncludeWeekends),
 lists:map(fun(X)-> element(1,X) end, DateTimeRange);

create_range(From,To,Frequency, IncludeWeekends) ->
 case IncludeWeekends of
 true ->
 create_range(From,To,Frequency);
 false ->
 Dates = create_range(From,To,Frequency),
 lists:foldl(fun(X,Acc) ->
 case is_weekend(X) of
 true ->
 Acc;
 false ->
 Acc++[X]
 end
 end,[],Dates)
 end.

create_range({Fy,Fm,Fs},{Ty,Tm,Ts},Frequency) ->
 DateTimeRange = create_range({{Fy,Fm,Fs},{0,0,0}},{{Ty,Tm,Ts},{0,0,0}},Frequency),
 lists:map(fun(X)-> element(1,X) end, DateTimeRange);

create_range(From,To,Frequency) ->
 {{Fy,Fm,Fd},{Fhour,Fmin,Fsec}} = From,
 {{Ty,Tm,Td},{Thour,Tmin,Tsec}} = To,
 FromGregor = calendar:datetime_to_gregorian_seconds({{Fy,Fm,Fd},{Fhour,Fmin,Fsec}}),
 ToGregor = calendar:datetime_to_gregorian_seconds({{Ty,Tm,Td},{Thour,Tmin,Tsec}}),
 Dates = case Frequency of
 yearly ->
 add_year_until([FromGregor],ToGregor);
 quarterly ->
 add_quarters_until([FromGregor],ToGregor);
 monthly ->
 add_months_until(Fd,[FromGregor],ToGregor);
 _ ->
 Interval = get_interval_seconds(Frequency),
 lists:seq(FromGregor,ToGregor,Interval)
 end,
 lists:map(fun(X) ->
 calendar:gregorian_seconds_to_datetime(X)
 end, Dates).

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_quarters_until(List,Max) ->
 Last = get_last_date(List),
 {{Y,M,D},{H,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Last),
 Next = case M+3 < 13 of
 true ->
 Date = fix_day({Y,M+3,D}),
 calendar:datetime_to_gregorian_seconds({Date,{H,Min,Sec}});
 false ->
 case M of
 10 ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y+1,1,D}),{H,Min,Sec}});
 11 ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y+1,2,D}),{H,Min,Sec}});
 12 ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y+1,3,D}),{H,Min,Sec}});
 _ ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y,M+3,D}),{H,Min,Sec}})
 end
 end,
 case Next < Max of
 true ->
 add_quarters_until(List++[Next],Max);
 false ->
 List
 end.

fix_day({Y,M,D}) ->
 Last = calendar:last_day_of_the_month(Y,M),
 case D > Last of
 true ->
 {Y,M,Last};
 false ->
 {Y,M,D}
 end.

add_months_until(FirstDay,List,Max) ->
 Last = get_last_date(List),
 {{Y,M,_D},{H,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Last),
 Next = case M +1 < 13 of
 true ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y,M+1,FirstDay}),{H,Min,Sec}});
 false ->
 calendar:datetime_to_gregorian_seconds({fix_day({Y+1,1,FirstDay}),{H,Min,Sec}})
 end,
 case Next > Max of
 true ->
 List;
 false ->
 add_months_until(FirstDay,List++[Next],Max)
 end.

add_year_until(List,Max) ->
 Last = get_last_date(List),
 {{Y,M,D},{H,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Last),
 {{MaxYear,_,_},_} = calendar:gregorian_seconds_to_datetime(Max),
 case MaxYear < Y+1 of
 true ->
 List;
 false ->
 Next = calendar:datetime_to_gregorian_seconds({{Y+1,M,D},{H,Min,Sec}}),
 add_year_until(List++[Next],Max)
 end.

get_last_date(List) ->
 case List of
 [H|[]] ->
 H;
 [_H|_T] ->
 hd(lists:reverse(List))
 end.

get_interval_seconds(Frequency) ->
 case Frequency of
 seconds ->
 1;
 minutes ->
 60;
 hours ->
 60*60;
 days ->
 60*60*24;
 weeks ->
 60*60*24*7
 end.

is_weekend(Date) ->
 {D,_} = Date,
 Day = calendar:day_of_the_week(D),
 case Day of
 7 ->
 true;
 6 ->
 true;
 _ ->
 false
 end.

thirdWednesday({Y,M,_D}) ->
 % Algorithm suggested by Mark Ransom at stack overflow quesion 5421972
 DayNum = calendar:day_of_the_week({Y,M,1}),
 SubNum = 4-DayNum , % Wednesday = 3
 if (SubNum < 0 ) ->
 {Y,M,SubNum + 21} ;
 true ->
 {Y,M,SubNum + 14}
 end.
%Returns a factor that needs to be multiplied for day count convention adjustment
% last two arguments are tenor with F signifying From and T signifying To
%used a paper from net Named OpenGamma Interest rate instrument and Market convention guide. Can't gurrantee the credibility
conventions(Catom,{Fy,Fm,Fd},{Ty,Tm,Td}) ->
 case Catom of
% space left for some comment
 '30/360' ->
 (360 * ( Fy - Ty ) + 30 * ( Fm - Tm ) + ( Fd - Td ) ) / 360;

% space left for some comment
 'act/360' ->
 (Td-Fd)/360;

% space left for some comment
 'act/365_fixed' ->
 (Td-Fd)/365;
% space left for some comment

'act/act_isda' ->

% space left for some comment
 IsLeapFrom = calendar:is_leap_year(Fy),
 case
 IsLeapFrom of
 true -> DF = 366;
 false -> DF = 365
 end,

% space left for some comment
 case Fy of
 Ty ->
 day_difference({Fy,Fm,Fd},{Fy,12,31}) / DF + day_difference({Ty,01,01},{Ty,Tm,Td}) / DF;
 true -> % not same hence find for To year as well
 IsLeapTo = calendar:is_leap_year(Ty),
 case
 IsLeapTo of
 true -> DT = 366;
 false -> DT = 365
 end,
 day_difference({Fy,Fm,Fd},{Fy,12,31}) / DF + day_difference({Ty,01,01},{Ty,Tm,Td}) / DT
 end;

% space left for some comment
 'business/252' ->
 io:stream("For This functionality I need business calendar which is still in progress")
 end.

% space left for some comment
day_difference(From,To) ->
 calendar:date_to_gregorian_days(To) - calendar:date_to_gregorian_days(From).

