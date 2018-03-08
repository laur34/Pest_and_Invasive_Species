from Bio import SearchIO

filter_func = lambda hspobj: hspobj.ident_num / hspobj.aln_span >= 0.97

blast_qresults = SearchIO.parse("/media/laur/8b3e5647-ec62-46b4-acf8-3edd161f78f5/result.bls", "blast-xml")

for blast_qresult in blast_qresults:
    print(len(blast_qresult))
    blast_hsp = blast_qresult[0][0]
    filtered_qr = blast_qresult.hsp_filter(filter_func)
    if filtered_qr:
        for hit in filtered_qr:
            for h in hit:
                print(h.ident_num / h.aln_span, " ", h)
    else:
        print(blast_hsp.ident_num / blast_hsp.aln_span)


