For analysing BackBlaze drive stats files from BackBlaze

There are 2 options
1.Download the files from https://www.backblaze.com/cloud-storage/resources/hard-drive-test-data should be expanded and placed in the data directory. Then run buildbackblaze.R to create the file sumbackblaze.csv
2.Alternatively the file sumbackblaze.csv can be used

sumbackblaze.csv - The latest summary file created by buildbackblaze

buildbackblaze.R - Reads data from BackBlaze to create a file containing all drives

analysebackblaze.R - Analyses the summary data

