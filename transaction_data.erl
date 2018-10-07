-module(transaction_data).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("records.hrl").

-export([decode_json/1, encode_json/1]).

decode_json(Json) ->
  JsonStruct = struct:from_json(Json),
  lists:map(fun(S) ->
    #transaction{timestamp = struct:get_value(<<"timestamp">>, S),
                 amount = binary_to_float(struct:get_value(<<"amount">>, S)),
                 payment_method = binary_to_atom(struct:get_value(<<"payment_method">>, S), utf8)}
  end, JsonStruct).

encode_json(Transactions) ->
  Txs = lists:map(fun(T) ->
          {struct, 
            [
              {<<"timestamp">>, T#transaction.timestamp},
              {<<"amount">>, float_to_binary(T#transaction.amount, [{decimals, 2}])},
              {<<"payment_method">>, atom_to_binary(T#transaction.payment_method, utf8)}
            ]}
        end, Transactions),
  struct:to_json(Txs).
