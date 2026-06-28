

This repository contains the datasets and R scripts to replicate our results. 

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
