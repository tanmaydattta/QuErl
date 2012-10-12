-module(blang).
-compile(export_all).
-record(contract,{name,starting_date,maturity,rate,fixed_percantage}).
-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).
readFile(FileName)->
    {ok,A} = file:consult(FileName),
    readContract(hd(A),1,size(hd(A))).
readContract(A,N,Max) ->
    if (N =< Max )->
                Elem = element(N,A),
                case is_atom(Elem) of
                    true -> readAtomicContract({Elem}),
                        readContract(A,N+1,Max);
                    false -> readAtomicContract(Elem),
                        readContract(A,N+1,Max)
                end;
             true ->
                 ok
         end.
readAtomicContract(A) ->
    NameOrNown = element(1,A),
    IsContract = isValidContract(NameOrNown),
    case IsContract of
        true -> io:format("I wil call read contract ~n"),
                makeContract(A);
        false ->
            IsNown = isValidNown(NameOrNown),
            case IsNown of
                true -> io:format("I will write logic for nown ~n");
                false -> io:format("Entered something that is not thougth of yet!! ~n")
            end
    end.
isValidContract(Name)->
    lists:member(Name,[zcb]).
isValidNown(Name) ->
    lists:member(Name,['and']).
makeContract(C) ->
        Type = element(1,C),
        case Type of
            zcb ->
        io:format("I will make zero coupon bond now ~p~n",[C]);
        _else -> io:format("nothing")
    end.
 
 -record(r,{name,starting_date,maturity,rate,fixed_percantage}).
 
 go() -> ?rec_info(contract,#contract{}).

