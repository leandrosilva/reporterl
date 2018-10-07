compile:
	erlc *.erl
print_summary_by_date:
	erl -noshell -run reporterl_client print_summary_by_date -run init stop
