
process(Fname) :-
	read_file_to_codes(Fname, Codes, []),
	process_codes(Codes).

process_codes()
