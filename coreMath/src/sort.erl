%% @author Tanmay Datta
%% @doc Sorting modules  
%% @copyright Tanmay QerlLib
-module(sort).
-compile(export_all).

quickSort([]) -> []; 
quickSort([L]) -> [L]; 
quickSort([L|List]) ->
	quickSort(lists:filter(fun(X) -> 	X < L end,List)) ++ [L] ++ 
	quickSort(lists:filter(fun(X) ->
						X > L end, List))
	.
