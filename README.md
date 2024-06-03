# TCR_shiny
R Shiny App for TCR analysis

##### Adrian Larkeryd, June 2024


The application is available at: [tcr-shiny](https://software.icr.ac.uk/app/tcr-shiny-test)

To debug the application locally after cloning:

These packages need installing
library(ggplot2)
library(bslib)
library(prodlim)
library(survival)
options(ggrepel.max.overlaps = Inf)

```bash
Rscript -e "install.packages('shinyFiles', repos='https://cloud.r-project.org')" 
Rscript -e "install.packages('shinyjs', repos='https://cloud.r-project.org')" 
Rscript -e "install.packages('immunarch', repos='https://cloud.r-project.org')" 
```

Run the application:
```bash
Rscript -e 'shiny::runApp("./", launch.browser = TRUE)'
```

Creating a conda env to run it:
```bash
mamba init
mamba create -n shiny -c conda-forge r-base r-shiny r-ggplot2 r-bslib r-prodlim r-survival r-shinyFiles r-shinyjs r-immunarch
mamba activate shiny
Rscript -e "shiny::runApp('.', port = 3838, host = '0.0.0.0')"
```

