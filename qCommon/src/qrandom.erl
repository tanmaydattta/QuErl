%% @author Tanmay Datta
%% @doc Generates random variables from various distributions.
%% @copyright Tanmay QerLib
-module(qrandom).
-export([normaldf/1,nInverse/1,rejection/2,randomN/3,nrandKinderMon/3]).

%% @doc Generates Normal density function with mean 0 variance 1 (Standard Normal). As suggested by Larry Wasterman
%% @end
-spec normaldf(float()) -> float().

normaldf(X) -> 
	if 
		X > 0 ->
			TT = 1/(1+0.2316419*X)
			, Out = 0.319381530 * TT - 
				0.356563782 * TT * TT +
				1.781477937 * TT * TT * TT - 
				1.821255978 * TT * TT * TT * TT + 
				1.330274429 * TT * TT * TT * TT * TT
			, 1 - dnorm(X)*Out; 
		true -> normaldf(-X)
	end. 	
%% Helper function (distribution normal) 
dnorm(X) -> 
	(math:exp(- X*X/2)) / math:sqrt(2*math:pi()). 
%% @doc Finds x such that Probability of Z less than x-> input
%% @end
-spec nInverse(Probability::float()) -> float().
nInverse(Q) ->
	if 
		Q >= 0.5 -> nInverseHelp(Q);
		true -> -1 * nInverseHelp(1-Q)
	end.

nInverseHelp(Q) -> 
	P = 1- Q
	, Y = - math:log(2 * P)
	, if 
		P > 0.0000001 ->
		math:sqrt	(((4 * Y + 100 ) * Y + 205) * Y * Y /
	  	    	     	(((2 * Y + 56) * Y + 192) + Y + 131));
		true -> 
		math:sqrt	((( 2 + Y + 280) * Y + 572 ) * Y /
				((Y+144) * Y + 603))
	end.
%% @doc Rejection method to generate N values from a given PDF
%% @end
-spec rejection(GX::function(),Values::integer())-> List::list().
rejection(PDF,N) ->
	rejectionH(PDF,N,[]).
%% Helper functions for rejection method
rejectionH(_PDF,0,List) -> List; 
	   				
rejectionH(PDF,N,List) ->
	U = random:uniform(),
	Y = random:uniform(),
	Yval = PDF(Y),
	
			if
				U =< Yval -> 
					rejectionH(PDF,N-1,List++[Y]); 
				true -> rejectionH(PDF,N,List)
			end.
%% @doc Generate N number of normal random numbers 
%% @end
-spec randomN(N::integer(),Variance::float(),List::list()) -> ListN::list().
randomN(0,_Sigma,List) -> List;
randomN(N,Sigma,List) -> 
	U = 2 * random:uniform()-1,
	V = 2 * random:uniform()-1,
	R = U * U + V * V,
	if 
		R > 1 -> randomN(N,Sigma,List);
		R == 0 -> randomN(N,Sigma,List);
		true -> randomN(N - 1, Sigma, List++[( Sigma * V * math:sqrt(-2*math:log10(R)/R))])
	end.
%% @doc Generate a List of N Normal random numbers according to Kinderman and Monahan's suggested Ratio Method.
%% This is the an implementation of Leva's modification to the original K + M method 
%%
%% @end
-spec nrandKinderMon(N::integer(),Variance::float(),EmptyList::list()) -> List::list().
nrandKinderMon (0,_Sigma,List) -> List; 
nrandKinderMon (N,Sigma,List) ->
       %% Define constanst as per GSL specs 
	S  = 0.449871,
 	T  = 0.386586,
       	A  = 0.196, 
       	B  = 0.25472, 
	R1 = 0.27597, 
       	R2 = 0.27846, 
	U  = 1 - random:uniform(), 
	V  = random:uniform() - 0.5, 
	X = U - S, 
	Y = abs(V) - T, 
       	Q = X * X + Y * ( A * Y - B * X), 
	LogU = math:log10(U),
	if 
		Q > R1 ,Q > R2 ; V* V > -4 * U - LogU-> 
			nrandKinderMon(N-1,Sigma,List ++ [Sigma * V / U ]); 
		true -> nrandKinderMon(N,Sigma,List)
	end.

