from Bio import SeqIO

#original_file = "./both_derep.fas"
#corrected_file = "./corrected_derep.fas"
original_file ="./Arthropoda_EU_NS.fas"
corrected_file="./Arthropoda_wPST.fas"

with open("pests_from_online", 'r') as pests_online:
    speclist = pests_online.read().splitlines()

#pests_online = open("pests_from_online", 'r')
#speclist = pests_online.readlines()
#there are a few empty strings still in it, though.
#speclist=["cockatiel","milk","bread"]


with open(original_file) as original, open(corrected_file, 'w') as corrected:
    counter=0
    records = SeqIO.parse(original_file, "fasta")
    for record in records:
        if any([record.id.__contains__(sp) for sp in speclist]):
            record.id += "|PEST"
            print(record.id)
            counter +=1
        else:
            print("false")
    print(counter)

