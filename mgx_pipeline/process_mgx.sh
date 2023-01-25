SAMPLE=$1 # SRA run ID (e.g., SRR######)
BILR=$2 # path to bilirubin reductase bowtie2 index
HUMAN=$3 # path to human genome bowtie2 index
THREADS=$4 # Number of threads
OUT=$5 # path to output directory
TMP=$6 # Path to temporary directory

# Download reads and trim them using trimgalore
fasterq-dump-orig.3.0.2 -e $THREADS -t $TMP -O $OUT $SAMPLE
pigz -p $THREADS ${OUT}/${SAMPLE}_1.fastq
pigz -p $THREADS ${OUT}/${SAMPLE}_2.fastq
trim_galore -o $OUT -j $THREADS --paired ${OUT}/${SAMPLE}_1.fastq.gz ${OUT}/${SAMPLE}_2.fastq.gz

# Map reads to the human genome reference and remove likely contaminants
bowtie2 -p $THREADS -x $HUMAN -1 ${OUT}/${SAMPLE}_1_val_1.fq.gz \
    -2 ${OUT}/${SAMPLE}_2_val_2.fq.gz \
     | samtools view -bS | samtools fastq -@ $THREADS -f 12 -F 256 \
     -1 ${OUT}/${SAMPLE}_1.fq -2 ${OUT}/${SAMPLE}_2.fq - 

pigz -p $THREADS ${OUT}/${SAMPLE}_1.fq
pigz -p $THREADS ${OUT}/${SAMPLE}_2.fq


# Summarize total number of reads in the QC'd reads
cat ${OUT}/${SAMPLE}_1.fq.gz | seqkit -j $THREADS stats -T > ${OUT}/${SAMPLE}.stats 
cat ${OUT}/${SAMPLE}_2.fq.gz | seqkit -j $THREADS stats -T >> ${OUT}/${SAMPLE}.stats
TOTAL=$(head -n 2 ${OUT}/${SAMPLE}.stats | tail -n 1| cut -f 4)

# Map reads to the bilirubin reductase index
bowtie2 --no-unal -p $THREADS -x $BILR -1 ${OUT}/${SAMPLE}_1.fq.gz -2 ${OUT}/${SAMPLE}_2.fq.gz \
    | samtools view -bS | samtools sort > ${OUT}/${SAMPLE}.bilR.bam
COUNT=$(samtools idxstats ${OUT}/${SAMPLE}.bilR.bam | awk '{ SUM += $3} END {print SUM}')

# Print sample, reads mapped to bilirubin reductase, total reads in sample
echo -e "${SAMPLE}\t${COUNT}\t${TOTAL}"
echo -e "${SAMPLE}\t${COUNT}\t${TOTAL}" > ${OUT}/${SAMPLE}.mapping.summary

