%% @author Tanmay Datta
%% @doc Root Finder Algorithms 
%% @copyright Tanmay QerlLib
-module(rootFinder). 
-compile(export_all).
%%-export([bisection/3,bisection/4,newtonRalphson/3,newtonRalphson/4,limit/1,secant/3]).
%%This module provide the basic numerical methods for 
%%calculating the roots.
%% @doc Information about exported functions. 
%% @end 
-spec info() -> Information::string(). 
info () -> 
	io:format("Function: bisection(Function,High,Low). ~n
Description: This function calculates the roots of the function b/w the High and Low values. Note** Function sign should change b/h High and Low ~n
Function: bisection (Function , High, Low , Precision). ~n
Description: Takes extra value for precision. ~n").

%% @doc Bisection method without precsion
%% @end
-spec bisection(X::function(),Y::any(),Y::any()) -> float(). 
bisection(Fun,Up,Down) -> 
	bisection(Fun,Up,Down,0.00001).

%% @doc Bisection method with user defined precison value 
%% @end
-spec bisection(X::function(),Positive::any(),Negative::any(),Y::any()) -> float(). 
bisection(Fun,Up,Down,Precision) ->
	Mid = (Up + Down) / 2,
	ValueAtMid = Fun(Mid),
	ValueAtUp = Fun(Up),
	ValueAtDown = Fun(Down),
	if 
		abs(ValueAtUp-ValueAtDown) < Precision -> Up;
		ValueAtMid<0 -> bisection(Fun,Up,Mid,Precision);
		ValueAtMid>0 -> bisection(Fun,Mid,Down,Precision);
		true -> Mid
	end.

%% @doc Newton Ralphson method with user defined precison value with precison 
%% default as 0.00001.
%% @end
-spec newtonRalphson(Function::function(),DFunction::any(),Guess::any()) -> any(). 
newtonRalphson(Fun,DFun,Guess) ->
	newtonRalphson(Fun,DFun,Guess,0.00001).

%% @doc Newton Ralphson method with user defined precison value 
%% @end
-spec newtonRalphson(Function::function(),DFunction::any(),Guess::any(),Precision::float()) -> any(). 
newtonRalphson(Fun,DFun,Guess,Precision) ->
	ValueOfFun = Fun(Guess),
	ValueOfDerivative = DFun(Guess),
	NextValue = Guess - (ValueOfFun/ValueOfDerivative),
	if abs(NextValue-Guess) < Precision -> NextValue;
	   true -> newtonRalphson(Fun,DFun,NextValue,Precision)
	end.
limit(X) -> 
	Half = X / 2, 
	if Half ==X -> X; 
	   true -> limit(Half)
   end.
%% @doc Calculate differential from 
%% @end 
pSyntheticDiff() ->
	receive 
		{solve,Fun,X0,X1,Pid} ->
		       	Pid!(Fun(X1) - Fun (X0))/(X1-X0)
			, pSyntheticDiff();
		{shutdown} -> ok
	end.

 secant(Fun,X0,X1) ->
	Pid = spawn(?MODULE,pSyntheticDiff,[]),
		Value = Fun(X1)
		, 
		if 
			abs(Value) <0.00000000000001 ->
				X1; 
			true ->
				Pid!{solve,Fun,X0,X1,self()},
				receive
					X -> 
						Diff = X
				end
				,NextValue = X1 - Fun(X1)/Diff
				, secant(Fun,X1,NextValue)
	       end.

inverse(Func,A,B,Y) ->
	Fun = fun(X) -> Func(X) - Y end,
	bisection(Fun,B,A,0.000000000001). 
