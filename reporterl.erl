-module(reporterl).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("records.hrl").

-export([summarize/2]).

summarize(Transactions, by_date) ->
  summarize_by_date(Transactions);
summarize(Transactions, by_amount) ->
  summarize_by_amount(Transactions);
summarize(Transactions, by_payment_method) ->
  summarize_by_payment_method(Transactions).

summarize_by_date(Transactions) ->
  TxGroup = group_by(fun(T) ->
              {{date, get_date(T#transaction.timestamp)}, T}
            end, Transactions),
  Summary = lists:map(fun(Txg) ->
              {{date, Date}, Txs} = Txg,
              Sum = lists:foldl(fun(Tx, Acc) ->
                      {A, PM} = Acc,
                      {Tx#transaction.amount + A, unique(PM ++ [Tx#transaction.payment_method])}
                    end, {0.0, []}, Txs),
              {TotalAmount, PaymentMethods} = Sum,
              summary_data:create_summary_by_data({{date, Date}, TotalAmount, PaymentMethods})
            end, TxGroup),
  Summary2 = lists:sort(fun(A, B) ->
               A#summary_by_date.date < B#summary_by_date.date
             end, Summary),
  Summary2.

summarize_by_amount(_Transactions) ->
  not_implemented_yet.

summarize_by_payment_method(_Transactions) ->
  not_implemented_yet.

%% HELPER FUNCTIONS

get_date(Timestamp) ->
  {{Y, M, D}, _Time} = get_datetime(binary_to_list(Timestamp)),
  parse_int(Y) ++ "-" ++ parse_int(M) ++ "-" ++ parse_int(D).

get_datetime(Timestamp) ->
  Seconds = calendar:rfc3339_to_system_time(Timestamp),
  Localtime = calendar:system_time_to_local_time(Seconds, second),
  Localtime.

group_by(MapFunc, Transactions) ->
  Dict = lists:foldl(fun({K, V}, D) -> dict:append(K, V, D) end, dict:new(), [MapFunc(T) || T <- Transactions]),
  dict:to_list(Dict).

parse_int(I) ->
  case I < 10 of
    true -> "0" ++ integer_to_list(I);
    false -> integer_to_list(I)
  end.

unique(List) ->
  if length(List) > 1
    -> sets:to_list(sets:from_list(List));
    true -> List
  end.