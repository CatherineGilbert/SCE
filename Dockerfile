# Use a geospatial-enabled R base image
FROM rocker/geospatial:4.5.1

# Install required system dependencies for R packages and tools like APSIM
#RUN apt-get update -y && apt-get install -y  make pandoc zlib1g-dev libxml2-dev libicu-dev cmake libgdal-dev gdal-bin libgeos-dev libcurl4-openssl-dev libssl-dev libproj-dev libsqlite3-dev libudunits2-dev libx11-dev libnetcdf-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libjpeg-dev libpng-dev libtiff-dev git wget tar && rm -rf /var/lib/apt/lists/*

RUN apt-get update -y && apt-get install -y \
  zlib1g-dev \
  git \
  wget \
  tar \
  apt-transport-https \
  software-properties-common \
  ca-certificates \
  && rm -rf /var/lib/apt/lists/*

# Install Microsoft package feed for .NET
RUN wget https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    rm packages-microsoft-prod.deb

# Install .NET runtime (8.0 example — adjust if needed)
RUN apt-get update && apt-get install -y dotnet-runtime-8.0 && \
    rm -rf /var/lib/apt/lists/*

# Set up global R options for all sessions
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# Install R package manager tools
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.3")'

# Copy and restore the R project package environment
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'

# Copy APSIM-X files to the Docker image (version 2025.4.7717.0)
COPY next_gen_apsim/data.tar.gz /tmp/
COPY next_gen_apsim/control.tar.gz /tmp/
COPY next_gen_apsim/debian-binary /tmp/

# Extract APSIM files to a permanent location and clean up temp files
RUN tar -xzf /tmp/data.tar.gz -C / && \
    tar -xzf /tmp/control.tar.gz -C / && \
    rm /tmp/data.tar.gz /tmp/control.tar.gz /tmp/debian-binary

# Add APSIM-X to environment path
ENV APSIM_PATH=/usr/local/apsimx
ENV PATH=$APSIM_PATH:$PATH

# Set working directory
WORKDIR /srv/shiny-server/

# Copy the Shiny app code into the container
COPY . /srv/shiny-server/

# Make the Shiny server port available to the outside world
EXPOSE 3838

# Run the Shiny server
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=3838)'
