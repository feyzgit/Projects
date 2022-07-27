toplam = REDUCE wertv12( INIT val TYPE wertv12 FOR wa IN t_coldat WHERE ( anlg IN t_anlg_rng )
																          NEXT val = val + wa-btr1 ) )
