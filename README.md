
# hdIndep: A Powerful Bootstrap Test of Independence in High Dimensions

## Setup

1. Clone the repo
2. Open it in RStudio
3. On the right panel there is a new tab "Build" (next to "Git"). Click on it
4. Click Install button
5. (Optional): More -> Configure Build Tools -> Generate documentation with Roxygen

Alternatively:

1. Clone the repo
2. Execute in R console:
```r
install.packages(<path-to-cloned-repository>, repos=NULL)
```

Alternatively:

1. Generate a Github PAT token [here](https://github.com/settings/tokens)
2. Execute in R console:
```r
remotes::install_github("https://github.com/mauolivares/hdIndep.git",
                        ref = "main", # Or other branch, tag, or commit
                        auth_token = <token>)
```

After successful installation, you can use the functions from `hdIndep` repository
as with a usual package installed from CRAN:

```r
library(hdIndep)
?stepdown_RomanoWolf # Read the documentation
stepdown_RomanoWolf(...)
```

You can also run a tiny simulation in your terminal (~10 seconds) to verify that all is up and running. Simply navigate to the base directory, i.e., set the working directory to the directory containing this README, and execute

```
cd scripts
Rscript tiny_simulation.r
```

## Usage

The main functions to be used in empirical applications are `stepdown_RomanoWolf.r` and `BMB.cv.r`. Their source files are in `R/` directory. Their documentation could be read from source files or, after installing the hdIndep package, in RStudio with e.g.`?stepdown_RomanoWolf`, like with usual functions.

The example usage of whole workflow is in `scripts/tiny_simulation.r`. 


## Data

The code `vignettes/emp_app` replicates the empirical application in this paper. We use data from two sources:

1. Hughes et al. (2009) collected liver tissue samples from mice at hourly intervals over a 48-hour period, pooling samples from 3-5 mice at each point. These data are available from Gene Expression Omnibus (GEO). In this paper, we focus on liver dataset (accession GSE11923). We extracted the data using the GEOquery and BiocManager R Packages.

2. When comparing the transcripts identified by our method against those from Hughes et al. (2009), we use the transcripts as reported by the authors in the original study. This data set is available from Hughes et al. (2009); see Table S1 in Associated Data section of their paper. The data set was downloaded from the link https://pmc.ncbi.nlm.nih.gov/articles/PMC2654964/#s2 . The final data set is available in `data/pgen.1000442.s012.xls`. 

Data Citation:

Hughes, M. E., DiTacchio, L., Hayes, K. R., Vollmers, C., Pulivarthy, S., Baggs, J. E., Satchidananda, P. & Hogenesch, J. B. (2009). Harmonics of circadian gene transcription in mammals. PLoS genetics, 5(4), e1000442.







