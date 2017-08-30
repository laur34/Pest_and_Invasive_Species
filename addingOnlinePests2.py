from Bio import SeqIO

original_file ="./Arthropoda_EU_NS.fas"
corrected_file="./Arthropoda_wPST.fas"

with open("pests_from_online", 'r') as pests_online:
    speclist = pests_online.read().splitlines()


with open(original_file) as original, open(corrected_file, 'w') as corrected:
    counter=0
    records = SeqIO.parse(original_file, "fasta")
    for record in records:
        if any([record.id.__contains__(sp) for sp in speclist]):
            record.id += "|PEST"
            record.description += "|PEST"
            counter +=1
        SeqIO.write(record, corrected, 'fasta')
    print(counter)

