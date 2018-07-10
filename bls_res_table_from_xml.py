from Bio import SearchIO
import sys

outfile = open("blastres_table.tsv", 'w')

blast_qresults = SearchIO.parse(sys.argv[1], 'blast-xml')

outfile.write("Query_name\tRef_ID\tRef_descr\te_value\tAlign_len\tPct_ident\n")

for blast_qresult in blast_qresults:
    print("%i %s %s" % (len(blast_qresult), " hits in db for ", blast_qresult.id))
    if len(blast_qresult) > 0:
        blast_hsp = blast_qresult[0][0]
        print(blast_hsp.query_id)
        print(blast_hsp.hit_id)
        print(blast_hsp.hit_description)
        print("%s %f" % ("evalue: ",blast_hsp.evalue))
        print("%s %s" % ("Algn. length ", blast_hsp.aln_span))
        print("%s %f" % ("Percent ident.: ", blast_hsp.ident_num / float(blast_hsp.aln_span)))
        outfile.write(str(blast_hsp.query_id) +'\t'+ str(blast_hsp.hit_id) + '\t' +str(blast_hsp.hit_description) + '\t' + str(blast_hsp.evalue) + '\t'+ str(blast_hsp.aln_span) +'\t'+ str(blast_hsp.ident_num / float(blast_hsp.aln_span)) +'\n')




outfile.close()
