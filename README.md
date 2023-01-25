# bilirubin-bioinfo repository

This repository contains the scripts related to the metagenomic analyses
presented in {Publication title}.

## Metagenomic processing pipeline
The mgx_pipeline directory contains a bash script for processing metagenomic
samples from SRA.
- mgx_pipeline/bilR/ : contains bilirubin reductase bowtie indices
- mgx_pipeline/process_mgx.sh : bash script for processing metagenome samples.

process_mgx.sh usage:
  process_mgx.sh {SRA_RUN_ID} {PATH to bilR index} {PATH to human genome index} {Threads} {Output Directory} {Temporary Directory}

  example: process_mgx.sh ERR1449717 ./bilR/bilR ./human_index/human 8 ./outdir /tmp/

## Metagenomic anaylsis script
The mgx_analysis directory contains the processed metagenomic data and metadata
and a R markdown notebook that contains the processing and analysis detailed in
the manuscript.
- mgx_analysis/bilirubin_mgx_data.csv : metagenomic data table for all samples included in study
- mgx_analysis/Bilirubin_MGX.Rmd : R markdown analysis notebook
- mgx_analysis/Bilirubin_MGX.pdf : rendered R markdown notebook with pregenerated figures
