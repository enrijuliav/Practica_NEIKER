import re
from Bio import Entrez

def parse_gene(file: str, gene: str) -> dict[str, list[str]]:
    """
    Esta función transforma el archivo resultante del análisis de hmmsearch en Galaxy en un diccionario con las secuencias encontradas.

    Input:
        Filepath del output del análisis de hmmsearch de Galaxy
        Str del gen que nos importa, nombre exacto
    Output:
        Diccionario cuyas claves son los IDs de cada muestra/secuencia y cuyos valores son listas con el siguiente formato:
            {ID: [Inicio-Final proteina DB, Alineamiento proteina DB, Inicio-final secuencia, Alineamiento secuencia]}

    En caso de alineamientos con múltiples dominios posibles alineados el resultado será:
        {ID: [I-F prot 1, Al prot 1, I-F sec 1, Al sec 1, I-F prot 2, Al prot 2, I-F sec 2, Al sec 2]}
    """

    with open(file) as f:
        doc = f.read()

    # Partimos el archivo según cada una de las secuencias, que son introducidas por ">> ", ignorando los datos iniciales (seqs[0])
    seqs = doc.split(">> ")[1:]

    # Output
    dicc = {}

    # Analizaremos secuencia a secuencia
    for seq in seqs:

        # Extraemos el ID
        ID = seq.split(" ")[0]

        # Dividimos según el dominio, en caso de que haya varios, eliminando los datos iniciales (domains[0])
        domains = re.split(r"domain [0-9]", seq)[1:]
        for domain in domains:
            # Extraemos, aprovechando el formato constante, los datos que nos interesan con una expresión regular
            alignment = re.findall(fr"{gene} +([0-9]+) ([A-z\.\*-]+) ([0-9]+) *\n.+\n *[0-9a-z_]+ +([0-9]+) ([A-z\.\*-]+) ([0-9]+) *\n.+PP", domain)
            # Añadimos la información de interés extraída con las expresiones regulares al diccionario en nuestro formato deseado
            if ID not in dicc:
                dicc[ID] = [f"{alignment[0][0]}-{alignment[-1][2]}", "".join([x[1] for x in alignment]), f"{alignment[0][3]}-{alignment[-1][5]}","".join([x[4] for x in alignment])]
            # En caso de que este sea el segundo dominio, añadimos la información a la del domino anterior
            else:
                dicc[ID].extend([f"{alignment[0][0]}-{alignment[-1][2]}", "".join([x[1] for x in alignment]), f"{alignment[0][3]}-{alignment[-1][5]}","".join([x[4] for x in alignment])])

    return dicc

def prepare_db(file: str) -> list[list[str]]:
    """
    Esta función transforma los archivo de bases de datos proteicas de NcycFunGen en un objeto parseado

    Input:
        Filepath de la base de datos de secuencias proteicas de NcycFunGen

    Output:
        Lista de listas, la primera contiene la descripción de cada una de las secuencias y la segunda, la seciencia proteica, con el formato:
        [[Descripciones], [Secuencias]]
    """
    
    with open(file) as f:
        doc = f.read()

    # Partimos el archivo según cada una de las secuencias, que son introducidas por ">", ignorando los datos iniciales (seqs[0])
    seqs = doc.split(">")[1:]

    # Output
    result= [[],[]]

    # Parseamos secuencia a secuencia
    for seq in seqs:
        # Extraemos la información de interés, ignorando las comillas que aparecen en algunas descripciones
        entry = re.findall(r"\"*([^\"]+)\"*\n(.+)",seq)
        result[0].append(entry[0][0])
        result[1].append(entry[0][1])

    return result

def parse_seqs(file:str) -> list[list[str]]:
    """
    
    """
    with open(file) as f:
        doc = f.read()

    # Output
    result= [[],[]]

    # Partimos el archivo según cada una de las secuencias, que son introducidas por ">"
    seqs = doc.split(">")[1:]

    for seq in seqs:
        entry = re.findall(r"(.+)\n(.+\n.*\n*.*)", seq)
        result[0].append(entry[0][0])
        result[1].append("".join(entry[0][1].split("\n")))

    return result

def write_phmmer(gene: str, hmmbuild: str, transeq: str) -> None:
    """
    Esta función escribe los datos en un archivo de texto de manera legible para al función de bash phmmer (fasta)

    Input:
        - gene: Una str conteniendo el nombre del gen que estamos analizando
        - hmmbuild: El objeto resultante de emplear la función parse_gene sobre el output de hmmbuild
        - transeq: El objeto resultante de la lectura con parse_seqs de las secuencias traducidas 6-frames
    
    Output:
        - None
        - Se crearán en esta carpeta los archivos hmmsearch_{gene}.fasta y hmmsearch_{gene}_aligned.fasta
    """
    with open(f"hmmsearch_{gene}.fasta", "w") as f:
        for sequence in hmmbuild:
            f.write(f">{sequence}\n")
            f.write(f"{transeq[1][transeq[0].index(sequence)]}\n")

    with open(f"hmmsearch_{gene}_aligned.fasta", "w") as f:
        for sequence in hmmbuild:
            f.write(f">{sequence}\n")
            f.write(f"{hmmbuild[sequence][3].upper()}\n")
    
    return

def extract_NIH_ID(file: str, mail: str) -> tuple[dict[str, str],dict[str, list[str]]]:
    """
    Esta función carga el archivo resultante del análisis de phmmer y, para cada sucencia, obtiene el ID del NIH de la secuencia que más se parece (la primera ordanada por E-value creciente)
    
    Input:
        - Nombre/path del archivo resultante de phhmer
    
    Output:
        - Diccionario con la estructura {"ID secuencia" : "ID NIH"}
    """

    Entrez.email = mail
    with open(file) as f:
        doc = f.read()

    seqs = re.split("Query: +", doc)[1:]

    IDs = {}

    results = {}

    for alignment in seqs:
        seq = re.match(r"[a-z0-9_]{34}", alignment).group()
        ID = re.search(r"-{11}\n.*([A-Z]{3}[0-9]{5}.[0-9]).*\n",alignment).groups()[0]
        IDs[seq] = ID

    for gene in IDs:
        stream = Entrez.efetch(db="protein", id=IDs[gene], rettype="gb", retmode="text")
        temp = stream.read().split("ORGANISM")[1]
        info = re.split(r"\n", temp)[0].strip()
        genus = re.split(r"\.\n", temp)[0].split(";")[-1].strip()
        results[gene] = [info,genus]
    
    return IDs,results


######################################################################################################################

outnirK = parse_gene("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/PHMMER/hmmbuild_out_nirK.txt", "nirK")
outnirS = parse_gene("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/PHMMER/hmmbuild_out_nirS.txt", "nirS")

protnirK = prepare_db("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Raw_data/HMM/Prot_database/nirK_prot_annot.fasta")
protnirS = prepare_db("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Raw_data/HMM/Prot_database/nirS_prot_annot.fasta")

transeqs = parse_seqs("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/transeq-otu-seqs.fasta")

write_phmmer("nirK", outnirK, transeqs)
write_phmmer("nirS", outnirS, transeqs)

""" phmmer -o nirK_phmmer.txt --tblout nirK_phhmer_table.txt --noali --cpu 20  hmmsearch_nirK.fasta Prot_database/nirK_prot_annot.fasta """
""" phmmer -o nirS_phmmer.txt --tblout nirS_phhmer_table.txt --noali --cpu 20  hmmsearch_nirS.fasta Prot_database/nirS_prot_annot.fasta """

""" phmmernirK_result = extract_NIH_ID("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/results_hmm/nirK_phmmer.txt", "enrijuliav@gmail.com")
NIH_ID_nirK = phmmernirK_result[0]
genus_nirK = phmmernirK_result[1]

phmmernirS_result = extract_NIH_ID("nirS_phmmer.txt", "enrijuliav@gmail.com")
NIH_ID_nirS = phmmernirS_result[0]
genus_nirS = phmmernirS_result[1]

with open("phmmer_nirK.txt", "w") as f:     
    for ID in NIH_ID_nirK:
        f.write(f">{ID}\n")
        f.write(NIH_ID_nirK[ID]+"\n")

with open("phmmer_nirS.txt", "w") as f:
    for ID in NIH_ID_nirS:
        f.write(f">{ID}\n")
        f.write(NIH_ID_nirS[ID]+"\n")

with open("genus_nirK.txt", "w") as f:     
    for ID in genus_nirK:
        f.write(f">{ID}\n")
        f.write("; ".join(genus_nirK[ID])+"\n")

with open("genus_nirS.txt", "w") as f:
    for ID in genus_nirS:
        f.write(f">{ID}\n")
        f.write("; ".join(genus_nirS[ID])+"\n") """


""" with open("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/feature-table.tsv") as f:
    doc = f.read()

with open("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/results_hmm/genus_nirS.txt") as f:
    doc = f.read() """

with open("/home/enrijuliav/Documentos/GitHub/Practica_NEIKER/Processed_data/HMM/results_hmm/genus_nirK.txt") as f:
    doc = f.read()

print(doc.split(">")[1:])
for seq in doc.split(">")[1:]:
    print(seq.split("\n")[:-1])
    print(seq.split("\n")[-2].split(";")[-1].strip())
