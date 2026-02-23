# Stage One

#Task One : GC-content Calculator
For the GC% calculation, make it robust enough to handle nucleotide sequences written in upper and lower case. I.e., GCATTTAT and gcaTTTAT should both return 25%.

# Create a function to calculate the GC content of a DNA sequence
GC_Calculator <- function(input_gene) {

  input_gene <- toupper(input_gene)     			#Standardise input sequence to UPPERCASE

  input_gene <- strsplit(x = input_gene, split = "")[[1]]  	#Split the string of DNA sequence into individual letters of nucleotides
  
  gc_counter <- 0						#Calculate the GC-content
    for (nuc in input_gene) {
    print(nuc)
    if (nuc == 'G' | nuc == 'C') {
      gc_counter = gc_counter + 1
    }
  }
  
  return((gc_counter / length(input_gene)) * 100)
  
}

gene1 = 'GCATTTAT'						#Uppercase sequence
GC_Calculator(input_gene = gene1)				#Calling the original function

gene2 = 'gcaTTTAT'						#Lowercase sequence
GC_Calculator(input_gene = gene2)				#Calling the original function



#Task Two: Write a function that returns the molecular weight of any protein in KiloDalton

#Create a function for calculating the weight of a protein sequence  
Protein_Weight_Calculator <- function(sequence) {

  sequence <- toupper(sequence)       #Standardize input to UPPERCASE

  aa_weights <- c(
    A = 71.08,  R = 156.19, N = 114.10, D = 115.09, C = 103.14,
    E = 129.12, Q = 128.13, G = 57.05,  H = 137.14, I = 113.16,
    L = 113.16, K = 128.17, M = 131.19, F = 147.18, P = 97.12,
    S = 87.08,  T = 101.11, W = 186.21, Y = 163.18, V = 99.13
  )                                   #Create a vector with the weights of all amino acids (minus the weight of H20)

  residues <- strsplit(sequence, split = "")[[1]]   #Split the string of protein sequence into individual letters of amino acids

  if (any(!(residues %in% names(aa_weights)))) {
    return(0)
  }                         #Check for invalid characters (i.e. non-amino acid characters like B, J, O, U, X and Z)

  total_weight_da <- sum(aa_weights[residues]) + 18.015  #Calculate the total weight (in Dalton)

  total_weight_kda <- total_weight_da / 1000            #Convert the value to KiloDalton (kDa)

  return(total_weight_kda)
}
protein1 <- "MARQKTYP"      #Valid protein sequence
Protein_Weight_Calculator(sequence = protein1)	#Calling the original function
protein2 <- "BRYAN"         #Invalid protein sequence (with non-amino acid characters)
Protein_Weight_Calculator(sequence = protein2)	#Calling the original function
