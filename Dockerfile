# https://towardsdatascience.com/deploy-your-rshiny-app-locally-with-docker-427386010eba
################### BUILD AND RUN THIS DOCKER FILE ############################

### BUILD IMAGE FROM root directory of project
# docker build -t icrsc/tcr-shiny .

### TEST IMAGE
# docker run --rm --name tcr -p 3838:3838 -v /mnt/c/Users/ralcraft/Documents/shiny-proxy-data/SYLVER/webapp:/app/webapp icrsc/tcr-shiny
# docker run --rm --name tcr -p 3838:3838 icrsc/tcr-shiny

### PUSH IMAGE TO DOCKER HUB
# docker push icrsc/tcr-shiny
#################################################################################

FROM condaforge/mambaforge:23.3.1-1 as conda

RUN apt-get update -y; apt-get upgrade -y; 
    
RUN conda create -n r-shiny -c conda-forge -y r-base r-shiny r-ggplot2 r-bslib r-prodlim r-survival r-shinyFiles r-shinyjs r-immunarch
RUN echo "alias l='ls -lah'" >> ~/.bashrc

# This is the conda magic. If you are running through a shell just activating the environment in your profile is peachy
RUN echo "source activate r-shiny" >> ~/.bashrc
ENV CONDA_EXE /opt/conda/bin/conda
ENV CONDA_PREFIX /opt/conda/envs/r-shiny
ENV CONDA_PYTHON_EXE /opt/conda/bin/python
ENV CONDA_PROMPT_MODIFIER (r-shiny)
ENV CONDA_DEFAULT_ENV r-shiny
ENV PATH /opt/conda/envs/r-shiny/bin:/opt/conda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# This is how we install custom R packages
# RUN R -e "install.packages('devtools', repos = 'http://cran.us.r-project.org')"

# Copy our app.R and required folders
COPY app.R ./
COPY www ./www
RUN chmod 777 app.R

EXPOSE 3838
CMD Rscript -e "shiny::runApp('.', port = 3838, host = '0.0.0.0')"

