

:- use_module(library(http/html_write)).


test_page(N) -->
	page([ title(['Test page'])
             ],
             [ h2(align(center),
                  ['Header 123']),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Col 1'),
                            th('Col 2')
                          ])
                     | \rows(N) % insert N rows
                     ])
             ]).

rows(0) -->!.
rows(N) -->
	html([ tr([ td(['Row #', N]),
                    td(em(N))
                  ])
             ]), !, {N1 is N-1}, rows(N1).

