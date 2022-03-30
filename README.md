# Semantic adaptation to the interpretation of gradable adjectives via active linguistic interaction
## Materials, data, and analysis code
Materials, data, and analysis code to reproduce the results described in the manuscript "Semantic adaptation to the interpretation of gradable adjectives via active linguistic interaction" by Sandro Pezzelle and Raquel Fernández submitted to the Journal of Memory and Language

Structure of the repo:

- README

- exp1

    - stimuli
    
        - borderline: contains 10 borderline images and corresponding captions
        
        - clearcut: contains 10 clearcut images and corresponding captions
    
    - raw_data: contains responses by each of the 20 participants 
    
    - aggregated_data: contains aggregated results (.csv) and the legend to understand the fields in it (.txt) along with the python script to build the .csv file from raw data (.py)
    
    - analysis_code: contains a single .R file to reproduce results and plots in the paper


- exp2

    - stimuli: contains 32 images used in exp2 and corresponding captions (.txt) and annotation (.json)
    
    - raw_data
    
        -   C: contains responses by each of the 20 participants in condition C(ontrol) and the legend to understand the fields
        -   Q: contains responses by each of the 20 participants in condition Q(uestion) and the legend to understand the fields
    
    - aggregated_data: contains aggregated results (.csv) and the legend to understand the fields in it (.txt)

        - questions: contains two files with the raw questions asked by each participant in the two presentation orders, A and B
    
    - analysis_code: contains a single .R file to reproduce results and plots in the paper
