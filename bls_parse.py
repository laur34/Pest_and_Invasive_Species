from Bio import SearchIO

out=open("fffff", 'w')
out.write("Query Name\tQuery Definition\tQuery Length\tHit ID\tHit Defintion\tHit Length\teValue\n")

filter_func = lambda hspobj: hspobj.ident_num / hspobj.aln_span >= 0.97
filter_ev = lambda hspobj: hspobj.evalue <= 10e-6
filter_len = lambda hspobj: hspobj.aln_span >= 300

blast_qresults = SearchIO.parse("/media/laur/8b3e5647-ec62-46b4-acf8-3edd161f78f5/result.bls", "blast-xml")

for blast_qresult in blast_qresults:
    print(len(blast_qresult))
    filtered_f = blast_qresult.hsp_filter(filter_func)
    filtered_ev = filtered_f.hsp_filter(filter_ev)
    filtered_len = filtered_ev.hsp_filter(filter_len)
    print(filtered_len)
    for hit in filtered_len.hits:
        for hsp in hit.hsps:
            fields = [filtered_len.id, filtered_len.description, str(filtered_len.seq_len), hit.id, hit.description, str(hit.seq_len), str(hsp.evalue)]
            out.write("\t".join(fields) + "\n")


out.close()
