Overview
------------

This is the accompanying code repository for the paper Bartl et al. (2025) - ''Understanding Travel Behaviour Patterns and Their Dynamics: 
Applying Fuzzy Clustering and Age-Period-Cohort Analysis on Long-Term Data of German Travellers''.

The repository contains the R code for all analyses in the paper, including the supplementary material.
The data in this repository is a randomly drawn 20% sample of the full dataset, which we are not allowed to make freely available.
We thank [Forschungsgemeinschaft Urlaub und Reisen e.V.](https://reiseanalyse.de/home/) for allowing us to publish part of the data
under the [CC BY 4.0 license](https://creativecommons.org/licenses/by/4.0/).


Folder structure:

- Code: Code for the main analyses and the sensitivity analyses.
- Data: Above described sample from the full dataset.
- Graphics: Figures, produced on the full dataset, not only the sample data contained in this repository.
- Results: Practically empty folder in which the result objects will be saved when running the code


# Notes on how to run the code

1. In case you want to call the `Main_Analysis.R` code on a (Linux) server, you should
run the following call in a Terminal, from the parent folder:

```
R CMD BATCH Code/Main_Analysis.R &
```

2. The code file `Table_Data.Rmd` can be called via the (Linux) Terminal:

```
Rscript -e "rmarkdown::render('Code/Table_Data.Rmd')"
```
Also: `Table_Data.Rmd` can only be run after the main code has finished since it uses the
created `analysis_data` object as well as `Models_hard`.

3. The code is based on parallelization and currently only works on Unix-based systems
(Linux or Mac). It can be run under Windows using ![https://learn.microsoft.com/en-us/windows/wsl/install](WSL).