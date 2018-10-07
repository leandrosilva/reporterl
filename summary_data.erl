-module(summary_data).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("records.hrl").

-export([create_summary_by_data/1, encode_summary_by_data_json/1]).

create_summary_by_data(Summary) ->
  {{date, Date}, TotalAmount, PaymentMethods} = Summary,
  #summary_by_date{date = Date, total_amount = TotalAmount, payment_methods = PaymentMethods}.

encode_summary_by_data_json(Summaries) ->
  Ss = lists:map(fun(S) ->
       {struct, 
         [
           {<<"date">>, list_to_binary(S#summary_by_date.date)},
           {<<"total_amount">>, float_to_binary(S#summary_by_date.total_amount, [{decimals, 2}])},
           {<<"payment_methods">>, list_to_binary(atom_list_to_string(S#summary_by_date.payment_methods))}
         ]}
       end, Summaries),
  struct:to_json(Ss).

%% HELPER FUNCTIONS

atom_list_to_string(AtomList) ->
  lists:join(",", lists:map(fun(A) -> atom_to_list(A) end, AtomList)).
