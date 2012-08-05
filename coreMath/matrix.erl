%% @doc Basic module similar to array in standard erlang library.
%% @copyrights Tanmay Datta 
%% @end
-module(matrix).
-export([new/2]).
-import(array,[new/0]).
-compile([debug_info]).
-type matrix() :: [list()].
-spec(new(Nrows::integer(),Ncols::integer()) -> matrix()).
new(R,C)-> newMat(R,C,[]). 
%% @doc This function works similar to array:new/1 and create a Nrows X Ncolumn %% matrix with all element undefined 
%% @end
newMat(0,_,Mat) -> Mat; 
newMat(R,C,[]) -> newMat(R-1,C,[lists:duplicate(C,0)]); 
newMat(R,C,[Mat]) -> newMat(R-1,C,[Mat|[lists:duplicate(C,0)]]); 
newMat(R,C,[MatR1|[Rest]]) -> newMat(R-1,C,[MatR1|Rest|[lists:duplicate(C,0)]]]). 

