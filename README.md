This repository contains the scripts and datasets for the analyses presented in **Pilvik, Maarja-Liisa (2021). Comparing the productivity of Estonian deverbal suffixes -_mine_, -_us_, and -_ja_ in five registers: a quantitative usage-based approach. _Eesti ja soome-ugri keeleteaduse ajakiri ESUKA / Journal of Estonian and Finno-Ugric Linguistics JEFUL_ x(x), xxx-xxx (to appear)**. The article compares the morphological productivity of three Estonian deverbal suffixes (-*mine*, -*us*, and -*ja*) in 5 different registers using the measures of realized, potential, and expanding productivity (see e.g., Baayen 1992, 1993, Baayen & Renouf 1996). Potential paradigmatic support for productivity is also discussed. Data for the study is gathered from different language corpora and the registers compared are scientific texts (SCI), newspaper texts (NEWS), fiction (FICT), contemporary spoken spontaneous language (SP), and archaic spoken dialects (DIA).

The repository contains 4 folders:  

- **[sample files](./sample_files)**  
	+ There are 1-2 sample files for each (sub)corpus. These files can be used to test the script *[script1_sample_files.R](./scripts/script1_sample_files.R)*.  
	+ The files for **the Balanced Corpus of Estonian** can be obtained from [here](https://www.cl.ut.ee/korpused/grammatikakorpus/index.php?lang=en). The files of the two spoken corpora ([**the Corpus of Estonian Dialects**](https://www.keel.ut.ee/et/keelekogud/murdekorpus) and [**the Phonetic Corpus of Estonian Spontaneous Speech**](https://www.keel.ut.ee/en/languages-resourceslanguages-resources/phonetic-corpus-estonian-spontaneous-speech)) are not publicly available. Access to the files can be requested by contacting the corpus administrators. The two sample files are published here with their permission.  
- **[scripts](./scripts)**  
	+ The scripts are numbered according to their relevance in the workflow.  
	+ **script1_sample_files.R** samples random files from the 5 (sub)corpora so that the total token count in each sample would be ~426,000 tokens.  
		- **Output**: samples_426k_filesNtokencounts.RData (names and sizes of the sampled files).  
	+ **script2_varcorp_uus_NEWS_filelists_subcorpora.R**, **script2_varcorp_uus_FICT_filelists_subcorpora.R**, **script2_varcorp_uus_SCI_filelists_subcorpora.R**, **script2_varcorp_uus_SP_filelists_subcorpora.R**, and **script2_varcorp_uus_DIA_filelists_subcorpora.R** extract all -*mine*, -*us*, and -*ja* nouns, verbs and all lemmas from the sample files, and clean the word lists from unwanted query results (only for the three suffixes).  
		- **Input**:  
			+ sample files in folders *sample_files*;   
			+ *samples426k_filesNtokencounts.RData* (names and sizes of the sampled files);  
			+ *SCI_mine_varcorp_all.csv*, *NEWS_mine_varcorp_all.csv*, *FICT_mine_varcorp_all.csv*, *SP_mine_varcorp_all.csv*, *DIA_mine_varcorp_all.csv* (previously manually corrected lists of -*mine* nouns in the corresponding register);  
			+ *SCI_ja_varcorp_all.csv*, *NEWS_ja_varcorp_all.csv*, *FICT_ja_varcorp_all.csv*, *SP_ja_varcorp_all.csv*, *DIA_ja_varcorp_all.csv* (previously manually corrected lists of -*ja* nouns in the corresponding register);  
			+ *SCI_us_varcorp_all.csv*, *NEWS_us_varcorp_all.csv*, *FICT_us_varcorp_all.csv*, *SP_us_varcorp_all.csv*, *DIA_us_varcorp_all.csv* (previously manually corrected lists of -*us* nouns in the corresponding register).  
		- **Output**:  
			+ *varcorp_uus_SCI_filelists.RData*, *varcorp_uus_NEWS_filelists.RData*, *varcorp_uus_FICT_filelists.RData*, *varcorp_uus_SP_filelists.RData*, *varcorp_uus_DIA_filelists.RData* (the initial word lists compiled from the files);
			+ *varcorp_uus_SCI_filelists_corr.RData*, *varcorp_uus_NEWS_filelists_corr.RData*, *varcorp_uus_FICT_filelists_corr.RData*, *varcorp_uus_SP_filelists_corr.RData*, *varcorp_uus_DIA_filelists_corr.RData* (the corrected word lists compiled from the files).   
	+ **script3_varcorp_uus_permutations.R** permutes the sample file ordering 100 times. Each time, the files from each register are divided into 21 subcorpora of cumulatively increasing size (each subcorpus is 21,300 tokens larger than the previous subcorpus). In each subcorpus, type, token, and hapax counts are calculated for the suffix tokens and for all tokens in general. These counts are the basis for finding the realized, potential, and expanding productivity measures. The script also creates corrected lists of both suffix and verb tokens for the largest of the 21 subcorpora in each register (i.e., the word lists in the total sample in each register). This is done only during the first permutation, since the largest subcorpus for a single register has identical contents in all 100 permutations (i.e., it contains the same words, but in a different order).   
		- **Input**: 
			+ *varcorp_uus_SCI_filelists_corr.RData*, *varcorp_uus_NEWS_filelists_corr.RData*, *varcorp_uus_FICT_filelists_corr.RData*, *varcorp_uus_SP_filelists_corr.RData*, *varcorp_uus_DIA_filelists_corr.RData* (the corrected word lists compiled from the files);  
			+ *varcorp_uus_SCI_verbtypes.xlsx*, *varcorp_uus_NEWS_verbtypes.xlsx*, *varcorp_uus_FICT_verbtypes.xlsx*, *varcorp_uus_SP_verbtypes.xlsx*, *varcorp_uus_DIA_verbtypes.xlsx* (the previously manually corrected verb lists).  
		- **Output**: 
			+ *varcorp_uus_100_permutations.csv* (a data frame with type, token, and hapax counts, and the realized, potential, and expanding productivity measures for each suffix (3) in each subcorpus (21) of each register (5) in each permutation round (100), yielding altogether 3\*21\*5\*100 = 31,500 rows);  
			+ *varcorp_uus_maxverbcorr.RData* (the corrected word lists for the total samples).  
	+ **script4_varcorp_uus_perm_productivity.R** calculates the mean productivity values and the 95% confidence intervals based on the values obtained in the permutations, and creates visualizations.   
		- **Input**: *varcorp_uus_100_permutations.csv* (a data frame with productivity measures).  
		- **Output**: 
			+ *varcorp_uus_perm_prod_means.RData* (6 data frames with the mean values of type, token, and hapax counts, and realized, potential, and expanding productivity for each suffix (3) in each subcorpus (21) of each register (5); the values are averaged across the 100 permutations);   
			+ *varcorp_uus_perm_realized_prod.png*, *varcorp_uus_perm_potential_prod.png*, *varcorp_uus_perm_expanding_prod.png*, *varcorp_uus_perm_global_prod.png*, *varcorp_uus_perm_types_tokens_hapaxes.png* (visualizations of the productivity measures).  
	+ **script5_varcorp_uus_perm_gams_comparisons.R** interpolates potential productivity values all suffixes at equal number of suffix tokens and plots various comparisons.  
		- **Input**: *varcorp_uus_100_permutations.csv*, *varcorp_uus_perm_prod_means.RData*.  
		- **Output**: *varcorp_uus_perm_PbyNc_Ncf.png*, *varcorp_uus_perm_Pratios.png*, *varcorp_uus_perm_expvspot.png*, *varcorp_uus_perm_P_comparisons.png* (comparisons including potential productivity).  
	+ **script6_varcorp_uus_relativefreqs.R** visualizes the relative frequencies of derivations and their verbal bases, compares the relative frequencies of -*mine* and -*us* nouns derived from the same base, and compares the character sequencies with which the base stems for -*mine* nouns, -*us* nouns and verbs end.  
		- **Input**:  
			+ *varcorp_uus_maxverbcorr.RData* (the corrected word lists for the total samples);  
			+ *varcorp_uus_basevsderiv_lexemelist.xlsx* (the English translations for some base lexemes).  
		- **Output**:  
			+ *varcorp_uus_basevsderiv_grid.png* (verbal base frequencies *vs*. derivation base frequencies);  
			+ *varcorp_uus_mine_vs_us_base.png* (relative frequencies of -*mine* *vs*. -*us* nouns);  
			+ *varcorp_uus_baseendseq.png* (base stem ending frequencies among -*mine* nouns, -*us* nouns and verbs).  
- **[data](./data)**  
	+ All the input and output datasets.  
- **[figures](./figures)**  
	+ All the figures obtained in the analyses.  
