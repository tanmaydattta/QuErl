%% @author Tanmay Datta
%% @doc Matrix and some missing array functions
%% @copyright Tanmay QerlLib
-module(matrix).
-compile(export_all).
%%-export([matrix/2,matrix/3,arraySum/2,arrayProduct/2,info/0,matrixInverse/1,matrixRow/1,matrixColumn/1,determinant/1]).
-spec info() -> Information::string().
%% @doc Information about the module.
%% @end
info() ->
 io:format("Function: matrix(N,M) ~n
Description: Creates a N by M Matrix with all the elements uninitialized.~n
Function: matrix(N,M,List).~n
Description: Creates a N by M matrix from a list with NXM elements. ~n
Throws error if number of elemtns are not equal. ~n
Function: arraySum(array1, array2) -> array3 ~n
Description: Computes sum of elements of two arrays. Throws error if array size is not same ~n").

-spec matrix(Rows::integer(), Column::integer()) -> Matrix::list(list()).
 %% @doc Creates a Matrix of N rows and M columns.
 %% @end
 matrix(N,M) ->
 lists:duplicate(N,lists:duplicate(M,0)).
 -spec matrix(Rows::integer(), Columns::integer(),List::list()) -> Matrix::list(list()).
 %% @doc Creates a Matrix of N rows and M columns and initialize it with elements in list.
 %% @end
matrix(N,M,List) ->
 matrixH(N,M,List,[]).

-spec matrixInverse(Matrix::array()) -> Inverse::array().
%% @doc Calculates the inverse of the matrix.
%% @end
matrixInverse(_Matrix) ->
 io:format("In progress").

-spec arraySum(Array1::array(),Array2::array())-> Array3::array().
%% @doc Calculates the sum of two arrays.
%% @end
arraySum(Array1,Array2) ->
 arraySumH(Array1,Array2,0,[]).
-spec arrayProduct(Array1::array(),Array2::array())-> Array3::array().
%% @doc Calculates the product of two arrays.
%% @end
arrayProduct(Array1,Array2) ->
 arrayProductH(Array1,Array2,0,[]).

-spec determinant(Matrix::array()) -> Determinant::any().
%% @doc Returns the determinant
%% @end
determinant(Matrix) ->
		 [NRows,NColumn] = dimensions(Matrix),
		 IsSquare = (1== (NRows/NColumn)),
		 case IsSquare of
			 true ->
				 case NRows of
					 1 ->
						 lists:nth(1,lists:nth(1,Matrix));
					 2 -> %% a00a11 - a01a10 where FlatMatrix = [a0,a1,a2,a4]
						 FlatMatrix = lists:flatten(Matrix),
						 hd(FlatMatrix)*lists:last(FlatMatrix) - lists:nth(2,FlatMatrix)*lists:nth(3,FlatMatrix);
					%% 3 -> array:map(fun(X,Value) -> case X rem.
					 _N -> 
			FirstRow = lists:foldl(fun(X,Acc)->L=lists:flatlength(Acc),if L rem 2 == 0 -> Acc ++ [X];true-> Acc ++[-X] end  end, [], hd(Matrix)),
			IndexList = lists:seq(1,lists:flatlength(FirstRow)),
			FirstRowTuple = lists:zipwith(fun(Fst,Index) -> {Index,Fst} end, FirstRow, IndexList),
			lists:sum(lists:zipwith(fun({Id,Val},Mat) -> Val * determinant(cofactor(1,Id,Matrix)) end, FirstRowTuple,Matrix)) 

					 end;
			 false -> io:format("Determinant in progresee ~n")
		 end.
-spec cofactor(M::integer(),N::integer(),Matrix::array()) -> CofactorMatrix::integer().
%% @doc Calculates the Cmn Cofactor matrix of a given matrix.
%% @end
cofactor(M,N,Matrix) ->
	[NRow,_NColumn] = dimensions(Matrix),
	Sign = (M * N) rem 2,  
			case NRow of 
				1 ->lists:nth(1,lists:nth(1,Matrix));
				2 -> lists:nth(2,lists:nth(2,Matrix));
			 _N->
				case Sign of 
				       0 -> removeMatrixColumn(N,removeMatrixRow(M,Matrix));
					_M -> removeMatrixColumn(N,removeMatrixRow(M,Matrix))
				end
			end.
-spec dimensions(Matrix::list(list())) -> List_of_Rows_and_Column::list().
dimensions(Matrix) ->
 Columns = lists:flatlength(lists:nth(1,Matrix)),
 Rows = lists:flatlength(Matrix) div Columns,
 [Rows,Columns].
removeMatrixRow(Row,Matrix) ->

 NewArrayList = lists:delete(lists:nth(Row,Matrix),Matrix),
 NewArrayList.
removeMatrixColumn(Column, Matrix) ->
	lists:map(fun(Row) -> dropN(Column,Row) end, Matrix). 
lowerMatrix([[A]]) -> [[1],[A]]; 
lowerMatrix([Row|Rows]) ->
	FirstElem = hd(Row),
       	FirstRow = Row, 
	FirstColumn = lists:map(fun(X) -> hd(X)/FirstElem end, [Row|Rows]), 
	ReduceMatrix = removeMatrixColumn(1,removeMatrixRow(1,[Row|Rows])),
	[FirstRow,FirstColumn]++ReduceMatrix. 	
transpose(Matrix) ->
	transposeH(1,Matrix,[]). 

transposeH(I,Matrix,CMatrix) ->
	case hd(Matrix) of 
		[] -> CMatrix;  
		_N -> NCmat = CMatrix ++ [lists:map(fun(X) -> hd(X) end, Matrix)],
	transposeH( I+1,lists:map(fun(X) -> tl(X) end,Matrix), NCmat) 
	end. 
vJoin(Matrix,[Col]) ->
	vcat(Matrix,[Col]);
vJoin(Matrix,[Col|RestCol]) ->
	NewMat = vcat(Matrix,Col),
	vJoin(NewMat,RestCol). 
hJoin(Matrix,[Col]) ->
	transpose(vcat(transpose(Matrix),[Col])); 
hJoin(Matrix,M2) ->
	transpose(vJoin(transpose(Matrix),M2)).

	 

%% Helper functions goes here. ----------------------------------------------------------------------------------------------------------------------------------
array(0,_M,Array) ->
 array:from_list(Array);

array(N,M,Array) ->
 array(N-1,M,Array++[array:new(M)]).

arraySumH(Array1,Array2,N,List) ->
 A1Element = array:get(N,Array1),
 A2Element = array:get(N,Array2),
 if
 is_number(A1Element), is_number(A2Element) ->
 arraySumH(Array1,Array2,N+1,List ++ [A1Element+A2Element]);
 true -> array:from_list(List)
 end.

arrayProductH (Array1,Array2,N,List) ->
 A1Element = array:get(N,Array1),
 A2Element = array:get(N,Array2),
 if
 is_number(A1Element) and is_number(A2Element) ->
 arrayProductH(Array1,Array2,N+1,List++[A1Element*A2Element]);
 true ->
 array:from_list(List)
 end.
 matrixH(0,_M,_List,Matrix) ->
 Matrix;

matrixH(N,M,List,Matrix) ->
 Elements = lists:flatlength(List),
 MatrixDim = N * M,
 Ratio = Elements / MatrixDim,
 case Ratio of
 1.0 ->
 FirstM = takeN(M,List),
 RestList = lists:subtract(List,FirstM),
 matrixH(N-1,M, RestList,Matrix++[FirstM]);
 _N -> io:format("Wrong dimensions ~n,~p~p~n",[MatrixDim,Elements])
 end.

takeNh(0,_List,Sublist) ->
		 Sublist;

 takeNh(N,List,Sublist) ->
		 takeNh(N-1,tl(List),Sublist++[hd(List)]).

 takeN(N,List) -> 
		 takeNh(N,List,[]).

 dropNH(1,List,FirstPart)->
		 FirstPart ++ tl(List);

 dropNH(N,[Head|Rest],FirstPart) ->
		 dropNH(N-1,Rest,FirstPart++ [Head]).
 dropN(N,List) ->
		 dropNH(N,List,[]).
ints_from(N) ->
    fun() ->
            [N|ints_from(N+1)]
    end.
vcat(Matrix,[[Col]|[RestCol]]) ->
	NewMat = vcat(Matrix,[Col]),
	vcat(NewMat,[[RestCol]]);
vcat(Matrix,[[Last]]) ->
	vcat(Matrix,[Last]);
vcat(Matrix,Col) ->
        	[R,C] = dimensions(Matrix),
		ColLength = lists:flatlength(Col),
		if 
			R < ColLength ->
				NewMat=hcat(Matrix,lists:duplicate(C,0)),
				PadCol = Col;
			true ->
				NewMat = Matrix,
			PadCol = Col ++ lists:duplicate(R-ColLength,0)
		end,

	lists:zipwith(fun(MRow,NewElem) -> MRow++[NewElem] end,NewMat,PadCol).
hcat(Matrix,Row) -> 
	transpose(vcat(transpose(Matrix),Row)). 
