%% @author Leandro Silva <leandrodoze@gmail.com>

-record(transaction, {timestamp, amount, payment_method}).
-record(summary_by_date, {date, total_amount, payment_methods}).