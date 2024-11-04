# PACKAGES ----------------------------------------------------------------------------------

# Make sure the following are installed. This includes packages that are not on CRAN
# and packages that are not loaded below but instead directly references in the code
# (to avoid having to load packages into memory of which we only use a few functions).
list.of.packages <- 
  c(
    "remotes", 
    "knitr", 
    "tidyverse",        # data wrangling and plotting 
    "magrittr",         # pipes, my friend, we need pipes 
    "assertthat",       # robust argument handling for functions 
    "fuzzyjoin",        # join tables based on relative matching
    "rlang",            # quosures and unquoting
    "ggforce",          # correlation matrices
    "ggnewscale",       # multiple scales
    "ggtext",           # vertical labels via geom_richtext
    "ggh4x",            # constant panel sizes across plots
    "patchwork",        # plotting multiple plots together
    "plotly",           # plotting 3D plots (in SI)
    "ellipse",          # covariance ellipses
    "linguisticsdown",  # IPA symbols
    "Cairo",            # plotting IPA and other special symbols
    # Doesn't interact well with JASA template since it automatically loads a number of 
    # latex packages, including pdflscape, which doesn't interact well with revtex 4.1,
    # which is used by the JASA template.
    # "kableExtra",       # styling tables (HTML formatting)
    "phonR",            # vowel normalization functions
    "phonTools",        # WattFabricius normalization function
    "brms",             # Bayesian mixed-effects multinomials regression
    "cmdstanr", 
    "mgcv",             # GAMMs
    "itsadug",          # GAMM visualization
    "modelr",           # making data grids for model predictions
    "tidybayes",        # working with posterior draws of Bayesian models
    "MVBeliefUpdatr",   # fitting and using Ideal Observers
    "future",           # parallel processing
    "furrr"             # parallel map fur future
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  if ("remotes" %in% new.packages) install.packages("remotes")
  if ("papaja" %in% new.packages) remotes::install_github("crsh/papaja")
  if ("cmdstanr" %in% new.packages) {
    install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    cmdstanr::install_cmdstan()
  }
  if ("MVBeliefUpdatr" %in% new.packages) remotes::install_github("hlplab/MVBeliefUpdatr")
  if ("supunsup" %in% new.packages) remotes::install_github("kleinschmidt/phonetic-sup-unsup")
  new.packages <- setdiff(new.packages, c("remotes", "papaja", "cmdstanr", "MVBeliefUpdatr", "supunsup"))

  install.packages(new.packages)
}

for (p in list.of.packages) {
  library(p, character.only = T)
}

# KNITR SETUP -------------------------------------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "../../figures/knitted/",
  dpi = 150,
  dev = c('cairo_pdf'), # default format of figures
  fig.align = "center",
  echo = FALSE, warning = TRUE, message = TRUE,
  cache = FALSE)

# some useful formatting functions for output of knitting
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

color_block = function(color) {
  function(x, options) sprintf('\\color{%s}\\begin{verbatim}%s\\end{verbatim}\\color{black}',
                               color, x)
}
knitr::knit_hooks$set(error = color_block('red'))
knitr::knit_hooks$set(warning = color_block('orange'))

# Get citation information
papaja::r_refs(
  file = "latex-stuff/r-references.bib",
  append = FALSE,
  type_pref = c("Manual", "Article", "Book"))

# CONSTANTS ----------------------------------------------------------------------------------
set.seed(333421864)

options(knitr.table.format = "latex")
base.width = 2.5
base.height = 2.5
base_size = 9

# FUCNTIONS ----------------------------------------------------------------------------------
source("constants-functions.R")
myGplot.defaults("paper")
