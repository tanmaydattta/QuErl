-module(interpolation).
-compile(export_all).
-spec (info() -> Information::string()). 
%% @doc Information about calling each functino in the module. 
%% @end
info() ->
	io:format("Functions :  interpolate  Usage : Pass at least two points in form {x,y} as interpolate ([{1,2},{3,4}], X) ~n
1. If X is passed then function will return you interpolate d value of Y. 
2. If X is missed then function returns a handle and you can pass any value to be interpolate d in it.
3. If X is a list then the values of corresponding Ys is calculated and the sorted {x,y} point pair is returned.  
~n"). 

interpolateH ([{X1,Y1},{X2,Y2}],X3) ->
	if 
		is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),is_number(X3) -> Y1 + (X3 - X1) * (Y2 - Y1) / (X2 - X1) ;
		true -> error("Interpolation not possible if all are not numbers")
	end.
interpolateH ([{X1,Y1},{X2,Y2}]) ->
	if 
		is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) -> 
			fun(X) -> Y1 + (X - X1) * (Y2 - Y1) / (X2 - X1) end ;
		true -> error("Interpolation not possible if all are not numbers")
	end.
interpolateH (List,XValues,check) ->
	XYList = xyTupleList(XValues),
       	FullList=lists:sort(List++XYList),
	FirstTupleY = element(2,erlang:hd(FullList)),
	case FirstTupleY of 
		undefined -> error("In Interpolation the first X/Y pair can not have undefined value of Y "); 
		_Else ->
			findinterpolationpairs(FullList,[])
	end.
findinterpolationpairs([],Values) -> Values; 
findinterpolationpairs([Pair|Rest],List) ->
	X = element(1,Pair),
	TupleY = element(2,Pair),
       case TupleY of 
       		undefined ->  
		Y =interpolateH ([lists:last(List),nextDefinedPoint(Rest)],X)
		,findinterpolationpairs(Rest,List++[{X,Y}]);
 		_else -> findinterpolationpairs(Rest,List++[Pair])
	end.	
nextDefinedPoint([Pair]) ->
	X = element(1,Pair), 
	Y = element(2,Pair), 
	case Y of 
		undefined -> error("Can not interpolateH H as last X/Y pair has undefined value"); 
		_Else -> {X,Y}
	end; 
nextDefinedPoint([Pair|Rest]) ->
	X = element(1,Pair), 
	Y = element(2,Pair), 
	case Y of 
		undefined -> nextDefinedPoint(Rest); 
		_Else -> {X,Y}
	end. 
%% @doc  This function converts List of XValues to a list of {X,undefined} kind of form
%% @end
xyTupleList(XValues) ->
	lists:map(fun(X) -> {X,undefined} end,XValues). 

tupleAppend(List,Tuple) ->
	IsTuple = is_tuple(Tuple),
	case IsTuple of 
		false -> " For Interpolation please enter X/Y pair in tuple ~n";
		true ->
			Size = erlang:tuple_size(Tuple), 
			case Size of 
				2 -> (List ++ [Tuple]);
				1 -> List ++ [{element(1,Tuple),y}];
				true-> error("Interpolation: tuple has to be X/Y pair ~n")
			end 
	end. 
interpolateHandle(List) ->
	fun(X) -> interpolateH (List,X,check) end. 
