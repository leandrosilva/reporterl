-module(reporterl_client).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("records.hrl").

-export([print_summary_by_date/0]).

print_summary_by_date() ->
  TransactionsJson = get_transactions_json(),
  Transactions = transaction_data:decode_json(TransactionsJson),
  Summaries = reporterl:summarize(Transactions, by_date),
  SummariesJson = summary_data:encode_summary_by_data_json(Summaries),
  io:fwrite("~s~n", [SummariesJson]).

%% HELPER FUNCTIONS

get_transactions_json() ->
  {ok, File} = file:open("transactions_fixture.json", [read]),
  try read_all_lines(File)
    after file:close(File)
  end.

read_all_lines(File) ->
  case io:get_line(File, "") of
      eof  -> [];
      Line -> Line ++ read_all_lines(File)
  end.