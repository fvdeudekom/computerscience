# computerscience
Paper Computer Science 

The paper is about finding duplicates using LSH. In LSH, the number of candidate pairs are reduced for a more quick detection. The candidate pairs are compared using Jaccard similarity. 

The code works as follows:
First, the titles of the data are obtained. 
Afterwards they are adjusted so that there representations are the same. 

For training LSH is performed where the LSH works as follows:
The model words are created and the brands are also added as model words. 
A binary vector is formed for all observations that contains information about the product containing the model words. 
A signature matrix is formed afterwards using min hashing. 
The candidate matrix is formed based on the values in the signature matrix. 
If two observations are in the same bucket for at least one band, they are marked as candidates. 

This candidates are evaluated as duplicates based on the jaccard similarity.
Also, candidates that have the same shop or have different brands are considered non-duplicates.
The threshold of similarity is tuned in the training and used in the testing part. 
The tuning was based on TP, FP, FN, and TN (or recall and precision). 

In the testing part, the tuned threshold is used in LSH to evaluate the duplicate detection. 
The performance of the duplicate detection and LSH is evaluated using relationship between PC and PQ and the fraction of comparisons.  



