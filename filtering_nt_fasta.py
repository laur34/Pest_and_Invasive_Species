#To parse the whole Genbank nt fasta file and look for species names on our list in the headers,
#creating a new fasta file of the sequences corresponding to species on our pest/invasive list.
#6.2018 LH

from Bio import SeqIO

input_file = "/media/laur/wdhdd1/nt"
id_file = "/home/laur/Pest_Inv_Arth_DB/Schaedlinge_fertig_list_all_spp.csv"
output_file = "ncbi_nt_schaedlinge.fasta"

with open(id_file) as id_handle:
    wanted = set(line.rstrip("\n") for line in id_handle)
print("Found %i unique identifiers in %s" % (len(wanted), id_file))

#For a large FASTA or FASTQ file, you would be better off not using the high-level SeqIO interface,
#but working directly with strings.
from Bio.SeqIO.FastaIO import SimpleFastaParser

count=0

with open(input_file) as in_handle:
    with open(output_file, "w") as out_handle:
        for title, seq in SimpleFastaParser(in_handle):
            if " ".join(title.split(None,1)[1].split(None)[0:2]) in wanted:
                out_handle.write(">%s\n%s\n" % (title, seq))
                count += 1


print("Saved %i records from %s to %s" % (count, input_file, output_file))
