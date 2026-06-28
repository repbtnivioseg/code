# Start from the official R base image matching the correct R version
FROM rocker/r-ver:4.4.2

# Install Linux system libraries required to build R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

# Create and set the container working directory
WORKDIR /project

# Copy the renv.lock file
COPY renv.lock ./

# Install renv and restore the exact package versions
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" \
    && R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); renv::restore(clean = TRUE)"

# Copy the rest of the project
COPY data/ ./data/
COPY *.r ./
