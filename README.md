

This repository contains the datasets and R scripts to replicate our results. 

test.r was used to produce figure 1 and the data for tables 1,2,3 and part of the data of table 5,6.

mil_civ.r was used for to produce figure 2 and part of the data for table 4.

mil_only.r was used to produce part of the data for table 4.

School_2009.r was used to produce figure 3 and part of the data of table 5.

decay_deaths.r was used to produce part of the data of table 6.


We present two options for replication.


## Option 1: Replicate Locally (renv)

1. Open R or RStudio in this project directory.
2. Install the environment manager (if needed):
   ```R
   install.packages("renv")
   ```
3. Restore the exact package dependencies:
   ```R
   renv::restore()
   ```
4. Run any R script (e.g., `test.r` or `school_2009.r`) to compute models and save plots.

---

## Option 2: Replicate Containerized (Docker)

1. Install Docker (if needed) from [docker.com](https://docs.docker.com/get-started/) and start the application.
2. Build the Docker image in your terminal:
   ```bash
   docker build -t troubles-segregation-analysis .
   ```
3. Run any script inside the container (e.g., `school_2009.r`):
   ```bash
   docker run --rm -v ${PWD}:/project troubles-segregation-analysis Rscript school_2009.r
   ```
   *(Note: The `-v` flag mounts your current directory so any generated plots are saved back to your computer).*
