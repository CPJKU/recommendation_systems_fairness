To carry out the significant testing the res folder (created when the first experiment is run) should be considered.
1. Run extract_data.ipynb to create the csv to be processed in R
2. Run Sig_accuracy_FM.R to perform significance testing for female vs male on accuracy metrics
3. Run Sig_Beyond_FM.R to perform significance testing for female vs male on beyond accuracy metrics
4. Run Sig_accuracy_settings.R to perform significance testing for standard vs debiased on accuracy metrics
5. Run Sig_Beyond_settings.R to perform significance testing for standard vs debiased on beyond accuracy metrics
6. Run muliple_comparisons_coverage.R and muliple_comparisons_diversity.R to perform significance testing across models on beyond accuracy metrics 
7. Run muliple_comparisons_accuracy.R to perform significance testing across models on accuracy metrics (recall and NDCG)
