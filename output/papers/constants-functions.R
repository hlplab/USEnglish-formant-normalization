# Constants ------------------------------------------------------------------
theme_set(
  theme_bw(base_size = 11) + 
    theme(
      panel.grid = element_blank()))

levels.vowel.IPA <- c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ʌ]", "[ʊ]", "[u]", "[ɑ]")
levels.vowel.Arpabet <- c("iy1", "ih1", "eh1", "ae1", "ah1", "uh1", "uw1", "aa1")

levels.Phase <- c("practice", "exposure", "test")
levels.Sex <- c("Female", "Male")
levels.Ethnicity <- c("Hispanic", "Non-Hispanic")
levels.Race <- c("American Indian", "Asian", "Black", "other", "White", "multiple")

# While R can handle unicode, stan cannot. Thus using IPA as values does not work for models
levels.response.vowel <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
labels.response.vowel <- c("i", "ɪ", "ɛ", "æ", "ʌ", "ʊ", "u", "ɑ")
levels.response <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
labels.response <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
levels.response.natural <- c("heed", "hid", "head", "had", "hut", "hood", "who'd", "odd")
labels.response.natural <- c("heed", "hid", "head", "had", "hut", "hood", "who'd", "odd")
levels.cue.names <- c("Item.Cue_Hz_F1", "Item.Cue_Hz_F2", "Item.Cue_Hz_F3", "Item.Cue_Mel_F1", "Item.Cue_Mel_F2", "Item.Cue_Mel_F3")
levels.formants <- c("F0", "F1", "F2", "F3")

levels.cue.names.transform <- c("F1_Hz", "F2_Hz", "F3_Hz", "F1_Mel", "F2_Mel", "F3_Mel", "F1_Bark", "F2_Bark", "F3_Bark", "F1_ERB", "F2_ERB", "F3_ERB", "F1_semitones", "F2_semitones", "F3_semitones")
levels.normalization <- c("r_Hz", "r_log", "r_Mel", "r_Bark", "r_ERB", "r_semitones", "SyrdalGopal_Bark", "SyrdalGopal2_Bark", "Miller_log", "Nearey2_log", "NordstromLindblom_Hz", "Johnson_Hz", "Nearey1_log", "CCuRE_Hz", "CCuRE_Mel", "CCuRE_Bark", "CCuRE_ERB", "CCuRE_semitones", "Gerstman_Hz", "Lobanov_Hz")
labels.normalization <- c("no normalization (Hz)", "transformed (log)", "transformed (Mel)", "transformed (Bark)", "transformed (ERB)","transformed (semitones)", "SyrdalGopal (Bark)", "SyrdalGopal2 (Bark)", "Miller (log)", "Uniform scaling, Nearey (log)", "Uniform scaling, Nordström & Lindblom (Hz)", "Uniform scaling, Johnson (Hz)", "Nearey's formantwise mean (log)", "C-CuRE (Hz)", "C-CuRE (Mel)", "C-CuRE (Bark)", "C-CuRE (ERB)", "C-CuRE (semitones)", "Gerstman (Hz)", "Lobanov (Hz)")
labels.normalization.numbered <- paste(1:length(labels.normalization), labels.normalization, sep = ". ")
levels.norm <- c("r", "SyrdalGopal", "SyrdalGopal2", "Miller", "CCuRE", "Nearey1", "Nearey2", "Gerstman", "Lobanov")

# Colors for normalization procedures
# We don't assign a color to "transformed (log)" because that approach is identical with Nearey's overall log-mean
# approach (but we need those values for the calculation of cumulative means in Study 2).
colors.all.procedures <- c("#100C08", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#E6BE8A", "#E6BE8A","#E6BE8A", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#DDADAF", "#DDADAF")
names(colors.all.procedures) <- labels.normalization

# Color codes from PhonR
#first define no of colors needed
num.col = 8
hue <- seq(0,  360, length.out=1+num.col)[-1]
chr <- seq(60, 100, length.out=num.col)
lum <- seq(60,  40, length.out=num.col)
pretty.col <- hcl(hue, chr, lum, alpha=1)
# Assign colors manually for greater distinguishability between categories
colors.vowels <- c("#BC854CFF", "#899000FF", "#009740FF", "#B705A8FF", "#0086B8FF", "#6D5BCCFF", "#009683FF", "#B61A51FF")
names(colors.vowels) <- levels.vowel.IPA
colScale <- scale_colour_manual(name = "Vowel",values = colors.vowels)

# Variable adjustments for plotting best-fitting params
colors.all.procedures.numbered <- colors.all.procedures
names(colors.all.procedures.numbered) <- labels.normalization.numbered

## STAN / BRMS constants ---------------------------------------------------------------
chains <- 4
options(
  width = 110,
  mc.cores = min(chains, parallel::detectCores()),
  cmdstanr_write_stan_file_dir = "../../models/")

require(brms)
my_priors <- c(
  prior(student_t(3, 0, 2.5), class = "b"),
  prior(cauchy(0, 2.5), class = "sd"),
  prior(lkj(1), class = "cor")
)

# Functions ------------------------------------------------------------------

## General functions ------------------------------------------------------------------
percent <- function(p) paste0(round(p * 100, 1), "%")
se <- function(x) sqrt(var(x) / (length(x) - 1))

scale_Gelman <- function(x) {
  (x - mean(x)) / (2 * sd(x))
}

my_unscale_Gelman <- function(data, scale_summary) {
  assert_that(nrow(scale_summary) == 1)
  
  cues <- intersect(names(data), c("F0", "F1", "F2", "F3", "Duration_CCuRE"))
  data %>%
    mutate(
      across(
        c(!!! syms(cues)),
        ~ .x * 2 * scale_summary[[glue("{cur_column()}_sd")]] + scale_summary[[glue("{cur_column()}_mean")]]))
}

geometric.mean = psych::geometric.mean

split_data_by_category <- function(
    data,
    proportion_data = .2
) {
  n_fold <- round(1 / proportion_data)
  
  data %>%
    group_by(category) %>%
    mutate(
      fold = base::sample(
        x = rep(1:n_fold, first(length(category)) %/% n_fold + 1), 
        size = first(length(category)), 
        replace = F))
}

# Function to identify whether tokens fall within ellipses
is_in_95_percent_region <- function(point, mean, sigma) {
  # Convert the mean and sigma matrix to a matrix
  mean <- matrix(mean, nrow = 1, ncol = 2)
  sigma <- matrix(sigma, nrow = 2, ncol = 2)
  
  # Calculate the Mahalanobis distance
  mahalanobis_distance <- mahalanobis(point, mean, sigma)
  
  # Calculate the 95th percentile chi-squared value
  chi_squared_95 <- qchisq(0.95, df = 2)
  
  # Check if the Mahalanobis distance is less than the 95th percentile chi-squared value
  return(mahalanobis_distance < chi_squared_95)
}

# Function for conducting t-tests
get_t_test <- function(
    x,
    y,
    dependent_var,
    grouping_var,
    alternative_hyp = "greater",
    data
) {
  
  t_test_result <- t.test(data[[dependent_var]][data[[grouping_var]] == x],
                          data[[dependent_var]][data[[grouping_var]] == y],
                          alternative = alternative_hyp, paired = TRUE, var.equal = FALSE)
  
  mean_difference <- mean(data[[dependent_var]][data[[grouping_var]] == y]) -
    mean(data[[dependent_var]][data[[grouping_var]] == x])
  
  tibble(
    x = x,
    y = y,
    Statistic = t_test_result$statistic,
    p_value = t_test_result$p.value,
    estimate_mean = mean(data[[dependent_var]]),
    mean_difference = mean_difference
  )
}

## Plotting functions ------------------------------------------------------------
myGplot.defaults = function(
    type = c("paper","poster","slides")[1],
    base_size = if (type == "paper") { 10 } else if (type == "slides") { 32 } else if (type == "poster") { 36 } else { 10 },
    margin=c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
    set_theme = T
)
{
  require(ggplot2)
  t <- theme(axis.text.x = element_text(size=base_size, vjust=1),
             axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
             axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
             axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
             strip.text = element_text(size=base_size, color = "white"),
             strip.background = element_rect(fill = "black", color = "black"),
             legend.title = element_text(size=base_size, face = "bold", hjust= 0),
             legend.text = element_text(size=base_size),
             plot.margin = unit(margin, "lines"),
             aspect.ratio = 1)
  
  if (set_theme) theme_set(theme_bw(base_size=base_size) + t) else return(t)
}

# Using grob.element
element_textbox_highlight <- function(
    ...,
    hi.labels = NULL, hi.fill = NULL,
    hi.col = NULL, hi.box.col = NULL,
    hi.labels2 = NULL, hi.fill2 = NULL,
    hi.col2 = NULL, hi.box.col2 = NULL,
    hi.labels3 = NULL, hi.fill3 = NULL,
    hi.col3 = NULL, hi.box.col3 = NULL,
    hi.labels4 = NULL, hi.fill4 = NULL,
    hi.col4 = NULL, hi.box.col4 = NULL
) {
  structure(
    c(ggtext::element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col,
           hi.labels2 = hi.labels2, hi.fill2 = hi.fill2, hi.col2 = hi.col2, hi.box.col2 = hi.box.col2,
           hi.labels3 = hi.labels3, hi.fill3 = hi.fill3, hi.col3 = hi.col3, hi.box.col3 = hi.box.col3,
           hi.labels4 = hi.labels4, hi.fill4 = hi.fill4, hi.col4 = hi.col4, hi.box.col4 = hi.box.col4)),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element",
              "element_textbox_highlight", "element_textbox", "element_text", "element",
              "element_textbox_highlight", "element_textbox", "element_text", "element",
              "element_textbox_highlight", "element_textbox", "element_text", "element"))
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
  }
  if (label %in% element$hi.labels2) {
    element$fill <- element$hi.fill2 %||% element$fill
    element$colour <- element$hi.col2 %||% element$colour
    element$box.colour <- element$hi.box.col2 %||% element$box.colour
  }
  if (label %in% element$hi.labels3) {
    element$fill <- element$hi.fill3 %||% element$fill
    element$colour <- element$hi.col3 %||% element$colour
    element$box.colour <- element$hi.box.col3 %||% element$box.colour
  }
  if (label %in% element$hi.labels4) {
    element$fill <- element$hi.fill4 %||% element$fill
    element$colour <- element$hi.col4 %||% element$colour
    element$box.colour <- element$hi.box.col4 %||% element$box.colour
  }
  NextMethod()
}

#Plot posterior grid/landscape plot
make_posterior_grid_from_io <- function(
    io,
    test.data,
    noise_treatment = "marginalize", # or to switch off noise: no_noise
    lapse_treatment = "marginalize",
    transform = "log",
    normalization = "Nearey2",
    talker = NA,
    resolution = 51,
    F1.limits = NULL,
    F2.limits = NULL
) {
  cues = paste0("F", 1:2, "_", transform, "_", normalization)
  if (is.null(F1.limits)) F1.limits <- range(test.data %>% { if (!is.na(talker)) filter(., Talker == talker) else . } %>% pull(!! sym(cues[1])))
  if (is.null(F2.limits)) F2.limits <- range(test.data %>% { if (!is.na(talker)) filter(., Talker == talker) else . } %>% pull(!! sym(cues[2])))
  
  m.io.landscape <-
    io %>%
    { if (is.na(talker))
      filter(., IO.cue_normalization == normalization, IO.cue_transform == transform) else
        filter(., IO.cue_normalization == normalization, IO.cue_transform == transform, IO.Talker == talker)
    } %>%
    crossing(
      F1 = seq_range(F1.limits, n = resolution),
      F2 = seq_range(F2.limits, n = resolution)) %>%
    mutate(x = map2(F1, F2, ~ c(.x, .y))) %>%
    nest(test_data = c(F1, F2, x)) %>%
    mutate(
      categorization = map2(
        test_data, io,
        ~ get_categorization_from_MVG_ideal_observer(
          x = .x$x,
          model = .y,
          decision_rule = "proportional",
          noise_treatment = noise_treatment,        
          lapse_treatment = lapse_treatment) %>%
          rename(posterior = response) %>%
          pivot_wider(names_from = category, values_from = posterior) %>%
          mutate(x = .x$x))) %>%
    mutate(
      test_data = NULL, 
      io = NULL) %>%
    unnest(categorization) %>%
    pivot_longer(
      cols = levels.vowel.IPA,
      values_to = "posterior",
      names_to = "IO.Vowel") %>%
    mutate(
      F1 = map(x, ~ .x[1]) %>% unlist(),
      F2 = map(x, ~ .x[2]) %>% unlist(),
      IO.Vowel = factor(IO.Vowel, levels = levels.vowel.IPA))
  
  m.io.landscape %>%
    ggplot(
      aes(
        x = F2,
        y = F1,
        fill = IO.Vowel,
        alpha = posterior)) +
    scale_x_reverse(paste0("F2 (", normalization, ")"), expand = c(0,0), position = "top") +
    scale_y_reverse(paste0("F1 (", normalization, ")"), expand = c(0,0), position = "right") +
    scale_alpha(range = c(0,1), limits = c(0,1)) +
    scale_colour_manual(name = "IO.Vowel", values = colors.vowels) +
    scale_fill_manual(name = "IO.Vowel", values = colors.vowels) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[1])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[2])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[3])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[4])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[5])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[6])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[7])) +
    geom_raster(data = . %>% filter(IO.Vowel == levels(factor(m.io.landscape$IO.Vowel))[8])) +
    guides(alpha = "none", color = guide_legend(nrow = 1)) +
    theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      aspect.ratio = 1) +
    ggh4x::force_panelsizes(cols = base.width, rows = base.height)
}

# Plot by-item analysis
make_by_item_plot <- function(data, normalization) {
    data  %>%
    filter(IO.NormalizationType == normalization) %>%
    ggplot(
      aes(
        x = log_likelihood_human,
        y = log_likelihood)) +
    stat_function(
      fun = I,
      geom = "line",
      color = "darkgray",
      linetype = "dotted") +
    geom_rug(
      data = . %>%
        group_by(Experiment) %>%
        summarise_at(vars(log_likelihood_human, log_likelihood), mean), sides = "bl",
      color = "blue") +
    geom_smooth() +
    #geom_smooth(method = "gam", formula = y ~ s(x), method.args = list(family = binomial)) +
    geom_point(
      data = . %>%
        filter(abs(likelihood_difference / (log_likelihood - likelihood_difference)) < .35),
      alpha = .5, size = 2, color = "gray") +
    #Add geom_path
    geom_path(
      # Filter down to item for which change is > .35, join in the full data in order to plot arrows
      data = . %>% 
        filter(abs(likelihood_difference / (log_likelihood - likelihood_difference)) > .35) %>%
        select(Experiment, ItemID, likelihood_difference, IO.NormalizationType, log_likelihood_human, log_likelihood) %>%
        left_join(data %>%
                    select(Experiment, ItemID, likelihood_difference, IO.NormalizationType, log_likelihood_human, log_likelihood), 
                  by = c("Experiment", "ItemID", "likelihood_difference")) %>%
        select(-c(IO.NormalizationType.x, log_likelihood_human.x, log_likelihood.x)) %>% 
        rename(IO.NormalizationType = IO.NormalizationType.y, log_likelihood_human = log_likelihood_human.y, log_likelihood = log_likelihood.y) %>% 
        distinct(),
      aes(
        group = ItemID, color = likelihood_difference),
      alpha = .9,
      arrow = arrow(ends = "last", type = "closed", angle = "22.5", length = unit(.2, "cm"))) +
    scale_color_gradient2("Difference in data log likelihood", low = "#CC0000", high = "#008000", mid = "#FFFAF0", midpoint = 0) +
    scale_x_continuous("Maximal possible performance", expand = c(0.01,0.01)) +
    scale_y_continuous("Log likelihood (of predicting *human response*)", expand = c(0.01,0.01)) +
    facet_wrap(~ Experiment) +
    theme(
      axis.title.y = element_markdown(),
      legend.position = "top")
  #coord_trans(trans = "asinh")
}

## Normalization functions ------------------------------------------------------------
F0_to_SR = function(F0) {
  168 * (F0 / 168)^(1/3)
}

F1_to_height = function(F1, SR) {
  log(F1 / SR)
}

F2_to_backness = function(F1, F2) {
  log(F2 / F1)
}

height_to_F1 = function(height, SR) {
  exp(height) * SR
}

backness_to_F2 = function(backness, height, SR) {
  exp(backness) * height_to_F1(height, SR)
}

get_transformation <- function(
  data,
  cues = c("F0", "F1", "F2", "F3")
) {
  data %>%
    mutate(
      across(
        c(!!! syms(cues)),
        c("log" = log,
          "Mel" = function(x) phonR::normMel(x),
          "Bark" = function(x) phonR::normBark(x),
          "ERB" = function(x) phonR::normErb(x),
          "semitones" = function(x) 12 * log2(x/100))))
}

# Takes a normalization name as argument, along with a named list of parameter vectors.
# The name of each list element is the name of the statistics that the parameters sum-
# marizes (e.g., "mean", "min", etc.). The elements of the vector are the values for 
# that statistic for all formants that are meant to be normalized (e.g., a 3-element 
# vector if F1-F3 are meant to be normalized by the returned function). 
# 
# The output of this function is a function that can take vectors of cues values as 
# input and returns vectors of normalized cue values.
# (Could potentially be extended to also take *lists* of cue values (i.e., multiple 
# observations) and then return lists of normalized cue values.)
get_normalization_function_from_parameters <- function(normalization, ..., verbose = F) {
  normalization_params <- list(...)[[1]]
  
  # if (verbose) {
  #   message("Returning normalization function for ", normalization, " with ", 
  #           length(normalization_params), " parameters ",
  #           paste(normalization_params, collapse = " and "))
  # }
  
  if (normalization %in% c("CCuRE", "Nearey1", "Nearey2")) {
    function(x) x - unlist(normalization_params[[1]])
  } else if (normalization %in% c("NordstromLindblom", "Johnson")) {
    function(x) x / unlist(normalization_params[[1]])
  } else if (normalization == "Gerstman") { 
    function(x) 999 * (x - unlist(normalization_params[[2]])) / 
      (unlist(normalization_params[[1]]) - unlist(normalization_params[[2]]))
  } else if (normalization == "Lobanov") {
    function(x) (x - unlist(normalization_params[[1]])) / unlist(normalization_params[[2]])
  } else message("Unrecognized normalization procedure: ", normalization)
}

# Assumes that F0, F1, F2, and F3 exist in the data.
# (if Mel, Bark, or alike are meant to be normalized, then the F0-F3_* columns need to be renamed to F0-F3)
get_normalization_functions_from_data <- function(
    data, 
    normalize_based_on_fold_types = c("training")
) {
  message(paste("Making formant normalization functions based on data in", 
                paste(normalize_based_on_fold_types, collapse = ", "), "folds."))
  
  talker_statistics <-
    data %>%
    filter(fold_type %in% normalize_based_on_fold_types) %>% 
    group_by(Talker) %>%
    summarise(
      formants_mean = list(c(mean(F0, na.rm = T), mean(F1, na.rm = T), mean(F2, na.rm = T), mean(F3, na.rm = T))),
      formants_min = list(c(min(F0, na.rm = T), min(F1, na.rm = T), min(F2, na.rm = T), min(F3, na.rm = T))),
      formants_max = list(c(max(F0, na.rm = T), max(F1, na.rm = T), max(F2, na.rm = T), max(F3, na.rm = T))),
      formants_sd = list(c(sd(F0, na.rm = T), sd(F1, na.rm = T), sd(F2, na.rm = T), sd(F3, na.rm = T))),
      formants_mean_log = list(c(mean(log(F0), na.rm = T), mean(log(F1), na.rm = T), mean(log(F2), na.rm = T), mean(log(F3), na.rm = T))),
      # For Nearey overall logmean *both* formants are normalized by the same quantity: 
      # the mean of the three means of log-transformed F1-F3. We thus make a 4-element 
      # vector with the same mean of log formant means.
      formants_overall_mean_log = list(rep(mean(c(log(F1), log(F2), log(F3)), na.rm = T), 4)),
      formants_vtl = list(rep(mean(F3[F1 > 600] / 2.5, na.rm = T), 4)),
      formants_vtl2 = list(rep(mean(c(F1 / 0.5, F2 / 1.5, F3 / 2.5), na.rm = T), 4)))
  
  # use constants for normalization functions, when needed
  f <- function(newdata) {
    message("Applying formant normalization functions to data.")
    newdata %<>%
      left_join(talker_statistics, by = "Talker") %>%
      mutate(
        formants = pmap(
          list(F0, F1, F2, F3), cbind),
        log_Nearey1 = map2(
          formants, formants_mean_log,
          ~ log(.x) - .y),
        log_Nearey2 = map2(
          formants, formants_overall_mean_log,
          ~ log(.x) - .y),
        Hz_NordstromLindblom = map2(
          formants, formants_vtl,
          function(x, y) (x / y)),
        Hz_Johnson = map2(
          formants, formants_vtl2,
          function(x, y) (x / y)),
        Hz_Gerstman = pmap(
          list(formants, formants_min, formants_max),
          function(x, y, z) 999 * ((x - y) / (z - y))),
        Hz_Lobanov = pmap(
          list(formants, formants_mean, formants_sd), 
          function(x, y, z) (x - y) / z),
        across(
          c(log_Nearey1, log_Nearey2, Hz_NordstromLindblom, Hz_Johnson, Hz_Gerstman, Hz_Lobanov),
          list("F0" = ~ unlist(map(.x, function(x) x[1])),
               "F1" = ~ unlist(map(.x, function(x) x[2])),
               "F2" = ~ unlist(map(.x, function(x) x[3])),
               "F3" = ~ unlist(map(.x, function(x) x[4]))), 
          .names = "{.fn}_{.col}"),
        # Add Miller 
        # (since this is intrinsic, normalizing params are taken from newdata rather than training_talker_statistics)
        F0_gm = geometric.mean(F0),
        F0_log_Miller = log(F0),
        F1_log_Miller = F1_to_height(F1 = .data$F1, SR = 168 * (.data$F0_gm / 168) ^ (1/3)),
        F2_log_Miller = F2_to_backness(.data$F1, .data$F2),
        F3_log_Miller = log(.data$F3 / .data$F2),
        # Add Syrdal-Gopal's Bark-distance model 
        # (since this is intrinsic, normalizing params are taken from newdata rather than training_talker_statistics)
        F0_Bark_SyrdalGopal = F0_Bark,
        F0_Bark_SyrdalGopal2 = F0_Bark,
        F1_Bark_SyrdalGopal = .data$F1_Bark - .data$F0_Bark,
        F1_Bark_SyrdalGopal2 = .data$F1_Bark - .data$F0_Bark, 
        F2_Bark_SyrdalGopal = .data$F2_Bark - .data$F1_Bark,
        F2_Bark_SyrdalGopal2 = .data$F3_Bark - .data$F2_Bark) %>%
      select(-c(F0_gm, Hz_Lobanov, log_Nearey1, log_Nearey2, Hz_NordstromLindblom, Hz_Johnson, Hz_Gerstman))
    
    return(newdata)
  }
  
  return(f)
}

# For legacy use only
get_normalization <- get_normalization_functions_from_data

# Assumes that the two formants are called F1 and F2 and that F0 and F3 exists, too.
# (if Mel, Bark, or alike are meant to be normalized, then the F0-F3_* columns need to be renamed to F0-F3)
get_C_CuRE_function <- function(
    data, 
    cues = levels.cue.names.transform
) {
  data %<>% 
    group_by(Talker) %>%
    summarise(
      across(
        .cols = .env$cues,
        .fns = list("overall_mean_for_CCuRE" = ~ mean(.x, na.rm = T)),
        .names = "{.fn}_{.col}"))

  f <- function(newdata) {
    require(glue)
    message(paste("Applying C-CuRE normalization to cues:", paste(cues, collapse = ",")))
    
    newdata %<>%
      left_join(data, by = "Talker") %>%
      mutate(
        across(
          .cols = .env$cues,
          .fns = list("CCuRE" = ~ .x - get(glue("overall_mean_for_CCuRE_{cur_column()}"))),
          .names = "{.col}_{.fn}")) #%>%
      #select(-starts_with("overall_mean_for_CCuRE_"))
    
    return(newdata)
  }
  
  return(f)
}
  

# Keep around for legacy use but get rid of it once it's not used anymore (to avoid confusion)
add_C_CuRE <- function(
    data, 
    normalize_based_on_fold_types = c("training"), 
    cues = levels.cue.names.transform
) {
  message(paste("Making C-CuRE normalization functions based on data in", normalize_based_on_fold_types, "folds."))
  return(get_C_CuRE_function(data %>% filter(fold_type %in% normalize_based_on_fold_types), cues = cues))
}

# Apply transformations and normalizations to the test data 
apply_all_transformations_and_normalization <- function(
    data, 
    normalize_based_on_fold_types
) {
  data %>%
    get_transformation() %>%
    get_normalization_functions_from_data(
      data = ., 
      normalize_based_on_fold_types = normalize_based_on_fold_types)() %>%
    rename(F0_Hz = F0, F1_Hz = F1, F2_Hz = F2, F3_Hz = F3) %>%
    add_C_CuRE(
      data = .,
      cues = c(
        "F0_Hz", "F1_Hz", "F2_Hz", "F3_Hz", 
        "F0_Mel", "F1_Mel", "F2_Mel", "F3_Mel", 
        "F0_ERB", "F1_ERB", "F2_ERB", "F3_ERB", 
        "F0_Bark", "F1_Bark", "F2_Bark", "F3_Bark", 
        "F0_semitones", "F1_semitones", "F2_semitones", "F3_semitones", 
        "Duration"),
      normalize_based_on_fold_types = normalize_based_on_fold_types)() %>%
    # Add '_r' for 'raw' to columns with scale-transformed data in order for pivoting to work in next chunk
    rename_with(
      .fn = ~ paste(.x, "r", sep = "_"), 
      .cols = matches(c("F[0-3].*Hz$", "F[0-3].*log$", "F[0-3].*Mel$", "F[0-3].*Bark$", "F[0-3].*ERB$", "F[0-3].*semitones$")))
}


## Outlier detection and correction --------------------------------------------------
get_cumulative_probability = function(x1, x2, mean, sigma) {
  pmvnorm(lower = -Inf, upper = c(x1, x2), mean = mean, sigma = sigma)
}

get_cumulative_probability_allCues = function(x1, x2, x3, x4, x5, mean, sigma) {
  pmvnorm(lower = -Inf, upper = c(x1, x2, x3, x4, x5), mean = mean, sigma = sigma)
}

get_cumulative_probability_univariate = function(x, mean, sd) {
  pnorm(x, mean = mean, sd = sd)
}

is_outlier = function(x, cutoff = outlier_probability_cutoff) {
  !between(
    x, 
    cutoff / 2, 
    1 - cutoff / 2)
}

obtain_densities <- . %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on F1 and F2
    x_F1F2 = map(data, ~ cbind(.$F1, .$F2)),
    mean_F1F2 = map(x_F1F2, ~ colMeans(.x)),
    cov_F1F2 = map(x_F1F2, ~ cov(.x)),
    # density based on height and backness
    x_HeightBackness = map(data, ~ cbind(.$height, .$backness)),
    mean_HeightBackness = map(x_HeightBackness, ~ colMeans(.x)),
    cov_HeightBackness = map(x_HeightBackness, ~ cov(.x)),
    x_F1F2 = NULL,
    x_HeightBackness = NULL) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_F1F2 = pmap(
      .l = list(F1, F2, mean_F1F2, cov_F1F2), 
      .f = get_cumulative_probability),
    cumulative_probability_HeightBackness = pmap(
      .l = list(height, backness, mean_HeightBackness, cov_HeightBackness), 
      .f = get_cumulative_probability)) %>%
  mutate_at(
    vars(cumulative_probability_F1F2, cumulative_probability_HeightBackness),
    unlist) %>%
  ungroup()

obtain_densities_Norm <- . %>%
  group_by(Talker, category, Normalization.Type) %>%
    nest() %>%
    mutate(
      # density based on F1 and F2
      x_F1F2 = map(data, ~ cbind(.$F1, .$F2)),
      mean_F1F2 = map(x_F1F2, ~ colMeans(.x)),
      cov_F1F2 = map(x_F1F2, ~ cov(.x))) %>%
    unnest(data) %>%
    # normalize densities within each talker and vowel
    mutate(
      cumulative_probability_F1F2 = pmap(
        .l = list(F1, F2, mean_F1F2, cov_F1F2), 
        .f = get_cumulative_probability)) %>%
    mutate_at(
      vars(cumulative_probability_F1F2),
      unlist) %>%
    ungroup()

#Obtain univariate densities for outlier correction of all cues in raw Hz
obtain_densities_univariates <- . %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on F0
    x_F0 = map(data, ~ cbind(.$F0)),
    mean_F0 = map(x_F0, ~ mean(.x, na.rm = TRUE)),
    sd_F0 = map(x_F0, ~ sd(.x)),
    # density based on F1
    x_F1 = map(data, ~ cbind(.$F1)),
    mean_F1 = map(x_F1, ~ mean(.x)),
    sd_F1 = map(x_F1, ~ sd(.x)),
    # density based on F2
    x_F2 = map(data, ~ cbind(.$F2)),
    mean_F2 = map(x_F2, ~ mean(.x)),
    sd_F2 = map(x_F2, ~ sd(.x)),
    # density based on F3
    x_F3 = map(data, ~ cbind(.$F3)),
    mean_F3 = map(x_F3, ~ mean(.x)),
    sd_F3 = map(x_F3, ~ sd(.x)),
    # density based on Duration
    x_Duration = map(data, ~ cbind(.$Duration)),
    mean_Duration = map(x_Duration, ~ mean(.x)),
    sd_Duration = map(x_Duration, ~ sd(.x)),
    x_F0 = NULL,
    x_F1 = NULL,
    x_F2 = NULL,
    x_F3 = NULL,
    x_Duration = NULL) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_F0 = pmap(
      .l = list(F0, mean_F0, sd_F0), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F1 = pmap(
      .l = list(F1, mean_F1, sd_F1), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F2 = pmap(
      .l = list(F2, mean_F2, sd_F2),
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F3 = pmap(
      .l = list(F3, mean_F3, sd_F3), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F3 = pmap(
      .l = list(F3, mean_F3, sd_F3), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_Duration = pmap(
      .l = list(Duration, mean_Duration, sd_Duration), 
      .f = get_cumulative_probability_univariate)) %>%
  mutate_at(
    vars(starts_with("cumulative_probability")),
    unlist) %>%
  ungroup()

#Obtain multivariate densities for outlier correction of all cues in raw Hz
obtain_densities_allCues <- function(data) {
  data %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on all cues
    x_cues = map(data, ~ cbind(.$F0, .$F1, .$F2, .$F3, .$Duration)),
    mean_cues = map(x_cues, ~ colMeans(.x)),
    cov_cues = map(x_cues, ~ cov(.x))) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_allCues = pmap(
      .l = list(F0, F1, F2, F3, Duration, mean_cues, cov_cues), 
      .f = get_cumulative_probability_allCues)) %>%
  mutate_at(
    vars(cumulative_probability_allCues),
    unlist) %>%
  ungroup()
}

# Function for training IOs
train_io <- function(cues, data, group = NULL, no_noise = T, n.category = length(unique(data$category)), verbose = F) {
  cue_transformation = unique(gsub("^_([A-Za-z]+)_.*$" , "\\1", cues))
  cue_normalization = unique(gsub("^.*_([a-z])$" , "\\1", cues))
  
  # Add noise_variance depending on the no of cues
  noise_variance <- if(no_noise) {
    rep(0, n_distinct(cues))
  } else {
    # in Mel
    rep(878, n_distinct(cues)) } 
  
  if (any(stringr::str_ends(cues, "_[cs]"))) {
    # calculate how much variance is changed compared to untransformed cues
    stats <-
      data %>%
      ungroup() %>%
      summarise_at(vars(contains("F")), sd) %>%
      select(cues, gsub("hertz", cues)) %>%
      pivot_longer(
        cols = everything(),
        names_pattern = "([A-Za-z]*)_(F[12])_([A-Za-z])",
        names_to = c("cue_transform", "formant", "cue_normalization")) %>%
      pivot_wider(names_from = "cue_normalization") %>%
      mutate(scale_factor = r / !! sym(cue_normalization))
    
    noise_variance <- noise_variance / stats$scale_factor
  }
  
  Sigma_noise <- diag(noise_variance)
  dimnames(Sigma_noise) <- list(cues, cues)
  
  priors <- rep(1/n.category, n.category)
  names(priors) <- sort(unique(data$category))
  
  m <- data %>%
    make_MVG_ideal_observer_from_data(
      verbose = verbose,
      category = "category",
      cues = cues,
      group = group,
      prior = priors,
      lapse_rate = 0,
      lapse_bias = priors,
      Sigma_noise = Sigma_noise
    )
  
  return(m)
}

## Finding optimal ideal observer parameterization -------------------------------------------
set_io_lapse_and_noise <- function(io, lambda, noise_multiplier) {
  io %>%
    mutate(
      lapse_rate = lambda,
      # For each category, get variance along each cue (e.g., F1 and F2). Take the
      # by-category geometric mean of those variances. Make a diagonal matrix with
      # each element of the diagonal set to that mean. Then take the mean of that
      # matrix across all categories, and multiply it by the IO.noise.multiplier.
      # (we're intentionally taking the arithmetic, rather than geometric, mean of
      # the variances since we want to keep this intuitive, and the noise multiplier
      # is meant only a measure relating the noise variance in a rather transparent
      # way to the average variance of all categories)
      Sigma_noise =
        map(Sigma, 
            function(.x) {
              .y <-
                cov2tau(.x) %>%
                `^`(., 2) %>%
                mean(.) %>%
                rep(., dim(.x)[1]) %>%
                diag(.) 
              
              # Since we've deconstruct the matrix, we've lost the dimnames.
              # So we are reassigning them
              dimnames(.y) <- dimnames(.x)
              return(.y)
            }
        ) %>%
        { reduce(., `+`) / length(Sigma_noise) * noise_multiplier } %>%
        list(.) %>%
        rep(., length(Sigma_noise)))
}

# Make function that calls MVBeliefUpdatr::evaluate_model() but also throws a
# message about what IO is currently being processed (to get incremental update
# messages).
my_evaluate_model <- function(IO.Type, model, x, response_category, decision_rule, noise_treatment, lapse_treatment, method, return_by_x) {
  message("Processing: ", IO.Type)
  evaluate_model(
    model = model,
    x = x,
    response_category = response_category,
    decision_rule = decision_rule,
    noise_treatment = noise_treatment,
    lapse_treatment = lapse_treatment,
    method = method,
    return_by_x = return_by_x)
}

get_likelihood_from_io <- function(io, x, response, lambda, noise_multiplier, method = "likelihood-up-to-constant") {
  # message("Calculating likelihood for lapse rate = ", lambda, ", noise rate = ", round(noise_multiplier*100, 1), "%")
  io %<>%
    set_io_lapse_and_noise(io = ., lambda, noise_multiplier)
  
  # Scale both the ideal observer and the input so that all models are evaluated with similar numerical accuracy
  # (since the scaling needs to be done over the sum of Sigma and Sigma_noise, we add them here and the no longer
  # consider noise below).
  s <- do.call(rbind, x) %>% cov() %>% diag() %>% { 1 / .}
  x <- map(x, ~ .x * s)
  io %<>%
    mutate(
      mu = map(mu, ~ .x * s),
      Sigma = map2(Sigma, Sigma_noise, ~ (.x + .y) * (s %*% t(s))))
  
  l <-
    evaluate_model(
      model = io,
      x = x,
      response_category = response,
      decision_rule = "proportional",
      noise_treatment = "no_noise",
      lapse_treatment = "marginalize",
      method = method,
      return_by_x = F)
  # message("...", l)
  return(l)
}

make_io_for_grid_search <- function(
    data,
    lambda = c(0, .01, .02, .04, .08, .16, .32),
    noise_multiplier = c(0, .2, .4, .8, 1.6, 3.2)) {
  
  io <- data %>%
    # Make IOs with different lapse rates and noise multipliers
    # a noise_multiplier rate of .1 means that the perceptual noise is set to 10% of the average category variance. 
    # By assumptions the perceptual noise is held to be identical across all cue dimension (in the normalized cue 
    # space)
    crossing(
      IO.noise_multiplier = noise_multiplier, 
      IO.lambda = lambda) %>%
    mutate(
      io = pmap(
        list(io, IO.lambda, IO.noise_multiplier), 
        ~ ..1 %>% 
          mutate(
            lapse_rate = ..2,
            # For each category, get variance along each cue (e.g., F1 and F2). Take the by-category mean of those variance.
            # Make a diagonal matrix with each element of the diagonal set to that mean. Then take the mean of that matrix 
            # across all categories, and multiply it by the IO.noise.multiplier.
            Sigma_noise =
              map(Sigma, 
                  function(.x) {
                    .y <-
                      cov2tau(.x) %>%
                      `^`(., 2) %>%
                      mean(.) %>%
                      rep(., dim(.x)[1]) %>%
                      diag(.) 
                    
                    # Since we've deconstruct the matrix, we've lost the dimnames.
                    # So we are reassigning them
                    dimnames(.y) <- dimnames(.x)
                    return(.y)
                  }
              ) %>%
              { reduce(., `+`) / length(Sigma_noise) * ..3 } %>% #Replaced noise_multiplier with ..3 - revert to old function
              #{ reduce(., `+`) / length(Sigma_noise) * noise_multiplier } %>%
              list(.) %>%
              rep(., length(Sigma_noise))))) %>%
    relocate(IO.Type, IO.lambda,  IO.noise_multiplier, IO.cues, IO.cue_normalization, IO.cue_transform, IO.crossvalidation_group, io) 
  message("Created all ideal observers.")
  return(io)
  
}

# Add the by-item information
get_by_item_accuracy_and_likelihood_from_io <-
  . %>%
  mutate(
    fit = future_pmap(
      .progress = T, 
      .options = furrr_options(
        packages = c("tidyverse"),
        seed = T),
      .l = list(paste(IO.lambda, IO.noise_multiplier, IO.cue_normalization, IO.cue_transform, IO.cues, Experiment), io, data_perception),
      .f = ~ my_evaluate_model(
        IO.Type = ..1,
        model = ..2,
        x = ..3$x,
        response_category = ..3$Response.Vowel,
        decision_rule = "proportional",
        noise_treatment = "marginalize",
        lapse_treatment = "marginalize",
        method = c("accuracy", "likelihood", "likelihood-up-to-constant"),
        return_by_x = T)),
    accuracy_Luce_by_item = map(fit, ~ .x$accuracy),
    log_likelihood_by_item = map(fit, ~ .x$likelihood),
    log_likelihood_up_to_constant_by_item = map(fit, ~ .x$`likelihood-up-to-constant`),
    accuracy_Luce = map_dbl(accuracy_Luce_by_item, ~ mean(.x$accuracy))
  ) %>%
  select(-c(fit))

get_likelihood_from_io_grid_search <- 
  . %>%
  map_dfr(
    . %>%
      mutate(
        fit = pmap(
          .progress = T, 
          .options = furrr_options(
            packages = c("tidyverse"),
            seed = T),
          .l = list(paste(IO.lambda, IO.noise_multiplier, IO.cue_normalization, IO.cue_transform, IO.cues, Experiment), io, data_perception),
          .f = ~ my_evaluate_model(
            IO.Type = ..1,
            model = ..2,
            x = ..3$x,
            response_category = ..3$Response.Vowel,
            decision_rule = "proportional",
            noise_treatment = "marginalize",
            lapse_treatment = "marginalize",
            method = c("accuracy", "likelihood-up-to-constant"),
            return_by_x = F)),
        accuracy_Luce = map(fit, ~ .x$accuracy),
        log_likelihood_up_to_constant = map(fit, ~ .x$likelihood)))


get_normalization_params <- function(data) {
  data %>%
    # Simple make one C-CuRE account for each transformation
    # (but call the log C-CuRE account "Nearey1", as we've done above)
    filter(StatisticName == "mean") %>%
    mutate(cue_normalization = ifelse(cue_transform == "log", "Nearey1", "CCuRE")) %>%
    # Make Nearey2 (single log mean)
    bind_rows(
      data %>%
        filter(StatisticName == "singlemean", cue_transform == "log") %>%
        mutate(cue_normalization = "Nearey2")) %>%
    # Make Nordström & Lindblom VTL
    bind_rows(
      data %>%
        filter(StatisticName == "singlemeanVTL", cue_transform == "Hz") %>%
        mutate(cue_normalization = "NordstromLindblom")) %>%
    # Make Johnson VTL
    bind_rows(
      data %>%
        filter(StatisticName == "singlemeanVTL2", cue_transform == "Hz") %>%
        mutate(cue_normalization = "Johnson")) %>%
    # Make Gerstman
    bind_rows(
      data %>%
        filter(StatisticName %in% c("min", "max"), cue_transform == "Hz") %>%
        mutate(cue_normalization = "Gerstman")) %>%
    # Make Lobanov
    bind_rows(
      data %>%
        filter(StatisticName %in% c("mean", "sd"), cue_transform == "Hz") %>%
        mutate(cue_normalization = "Lobanov"))
}

get_mu_n <- function(cum_sum, cum_n, mu_0, kappa_0) {
  (mu_0 * kappa_0 + cum_sum) / (kappa_0 + cum_n)
}

get_sigma_n <- function(cum_sd, cum_n, sigma_0, nu_0) {
  geometric.mean(c(rep(sigma_0, nu_0), rep(cum_sd, cum_n)))
}

# simple linear interpolation of normalization parameter based on strength of prior beliefs
# (prior_n) and amount of data seen from new input (cum_n)
get_parameter_n <- function(cum_param, cum_n, prior_param, prior_n) {
  if (prior_n == 0L) return(cum_param) else if (cum_n == 0L) return(prior_param)
  
  return((cum_param * cum_n + prior_param * prior_n) / (cum_n + prior_n))
}

# Could make this contingent on the StatsName (e.g., geometric means for SD)
# but for now we're just doing simple linear interpolation for all parameters
apply_cumulative_normalization <- function(data, normalization, kappa_0) {
  data %>%
    mutate(
      # Could make this contingent on the StatsName (e.g., geometric means for SD)
      # but for now we're just doing simple linear interpolation for all para-
      # meters. Note that we're exploring kappa_0 in log-space (since the same
      # changes in kappa_0 for large kappa_0s have less of an effect on the
      # posterior statistics than the same change for small kappa_0s). So we're
      # back-transforming the kappa_0 parameter here before applying it to get
      # the posterior statistics.
      PosteriorStatistic = pmap(
        .l = list(.data$SampleStatistic, .data$Trial, .data$PriorStatistic, .env$kappa_0),
        .f = ~ get_parameter_n(...))) %>%
    # Since some normalization procedures have more than one parameter, group the
    # data and hand the list of those parameters to the get_normalization_function
    group_by(ParticipantID, Trial, ItemID) %>%
    summarize(
      x = list(
        get_normalization_function_from_parameters(
          normalization,
          .data$PosteriorStatistic)(first(x))),
      Response.Vowel = first(.data$Response.Vowel),
      .groups = "drop")
}


## Functions for by item analysis ---------------------------------------------
get_by_item_data <- function(data1, data2) {
  data1 %>%
    select(Experiment, log_likelihood_up_to_constant_by_item, IO.NormalizationType, IO.crossvalidation_group, IO.lambda, IO.noise_multiplier) %>%
    unnest(log_likelihood_up_to_constant_by_item) %>%
    mutate(
      F1 = map_dbl(x, ~ .x[1]),
      F2 = map_dbl(x, ~ .x[2])) %>%
    select(-c(x, N)) %>%
    #unnest(data_perception) %>%
    left_join(
      data2 %>%
        select(Experiment, ItemID, log_likelihood_human, log_likelihood_chance, Response.Vowel_MostFrequent, starts_with("F"), Response.N) %>%
        pivot_longer(
          cols = -c(Experiment, ItemID, log_likelihood_human, log_likelihood_chance, Response.Vowel_MostFrequent, Response.N),
          names_to = c("formant", "cue_transform", "cue_normalization"),
          names_sep = "_",
          values_to = "Cue.Value") %>%
        pivot_wider(
          names_from = "formant",
          values_from = "Cue.Value") %>%
        mutate(
          IO.NormalizationType = paste(cue_normalization, cue_transform, sep = "_"),
          IO.NormalizationType = factor(plyr::mapvalues(IO.NormalizationType, levels.normalization, labels.normalization), levels = labels.normalization)) %>%
        select(-c(cue_transform, cue_normalization, F3, Response.N)),
      by = c("Experiment", "IO.NormalizationType", "F1", "F2")) %>%
    #summarise likelihood across the different lambdas and noise params
    group_by(Experiment, IO.NormalizationType, IO.crossvalidation_group, ItemID, log_likelihood_human, Response.Vowel_MostFrequent, F1, F2) %>%
    summarise(
      log_likelihood = mean(log_likelihood, na.rm = T)) %>%
    ungroup()
}


## Experiment list functions ---------------------------------------------
make_catch_tibble <- function(
  .data,
  blocks, #start to final block where catch trials should be added (in the following format - 2:6)
  penultimate_block
  ) {
  .data %>%
  group_by(exposure_condition) %>%
  mutate(exposure_materials = sort(rep(c("A", "B"), length.out = n()))) %>%
  group_by(exposure_condition, exposure_materials) %>%
  mutate(block = sort(rep(blocks, length.out = n()))) %>%
  ungroup() %>%
  arrange(exposure_condition, exposure_materials, block) %>%
  crossing(exposure_list = 1:5) %>%
  mutate(block = (block + exposure_list) %% penultimate_block + 2) %>%
  mutate(block_order = case_when(exposure_list == 1 ~ "ABCDE",
                                 exposure_list == 2 ~ "BCDEA",
                                 exposure_list == 3 ~ "CDEAB",
                                 exposure_list == 4 ~ "DEABC",
                                 exposure_list == 5 ~ "EABCD"))
  }


## Data preparation ------------------------------------------------------- 
simplifyAnswer <- function(x) {
  x = gsub("[|]", ";", x)
  x = ifelse(str_ends(x, ";"), x, paste0(x, ";"))
  ifelse(str_count(x, ";") > 1, "multiple", str_remove(x, ";"))
}


formatData_NORM <- function(.data, experiment) {
  require(assertthat)
  require(lubridate)
  
  experiment_labels <- c("1a (synthesized)", "1b (natural)")
  assert_that(experiment %in% experiment_labels)

  n.stims <- case_when(
    experiment == experiment_labels[1] ~ 146 * 2 + 1,
    experiment == experiment_labels[2] ~ 72 * 2 + 1)
  levels.response <- case_when(
    experiment == experiment_labels[1] ~ levels.response,
    experiment == experiment_labels[2] ~ levels.response.natural)
  
  .data %<>%
    # Remove any variables that are all NA
    mutate_at(
      vars(
        # variables that are constant and INFORMATIVE across all rows
        hittypeid, hitgroupid, title, description, keywords, assignments, assignmentduration, autoapprovaldelay, reward,
        starts_with("Qualification"),
        # variables that are constant and UNinformative across all row
        assignmentstatus, hitstatus, reviewstatus, numavailable, numpending, numcomplete, annotation,
        # variables that are NAs across all rows
        assignmentapprovaltime, assignmentrejecttime, deadline, feedback, reject),  
      function(x) x <- NULL) %>%
    # Renaming
    rename(
      Experiment.Protocol = Answer.rsrb.protocol,
      List = Answer.list_test,
      AssignmentID = assignmentid,
      Assignment.Accept.DateTime.UTC = assignmentaccepttime,
      Assignment.Submit.DateTime.UTC = assignmentsubmittime,
      # Was mean to store user time in GMT format. Not helpful since it's not actually the local time of the user.
      # Assignment.Submit.DateTime.UserLocalTime = Answer.userDateTime, 
      Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC = Answer.userDateTimeOffset) %>%
    # Add user local time (for now ignoring day light savings etc.)
    mutate(
      Assignment.Submit.DateTime.UserLocalTime = Assignment.Submit.DateTime.UTC - minutes(Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC),
      Assignment.Submit.DuringDayTime = ifelse(between(hour(Assignment.Submit.DateTime.UserLocalTime), 7, 21), T, F)) %>%
    { if ("Answer.us.timezone" %in% names(.)) 
          rename(., Assignment.Submit.US_TimeZone = Answer.us.timezone) else 
            mutate(., Assignment.Submit.US_TimeZone = NA) } %>%
    # Separate the practice, exposure, and test columns into one column per trial
    # (each with one more parts then there are trials because the last element is also followed by a ";". 
    # This last empty element should then be removed. If you get a warning, however, that means that at 
    # least one participant has more trials than expected)
    { if (all(c("Answer.practResp") %in% names(.)))     
      separate(., 
               Answer.practResp,
               into = paste0("Practice_Trial", 1:3),
               sep = ";") else . } %>%
    { if ("Answer.exposureResp" %in% names(.)) 
      separate(.,
               Answer.exposureResp,
               into = paste0("Exposure_Trial", 1:101),
               sep = ";") else . } %>%
    separate(
      Answer.testResp,
      into = paste0("Test_Trial", 1:n.stims),
      sep = "\\;") %>%
    pivot_longer(
      cols = contains("_Trial"), 
      names_to = c("Phase", "Trial"),
      names_pattern = "(.*)_Trial(.*)"
    ) %>%
    # Remove empty final trial from each phase
    filter(value != "") %>%
    # Separate trial-level information into multiple columns
    separate(
      value,
      into = c("CHECK.Phase", "Trial.ProvideFeedback",
               "CHECK.Trial", "ItemID", 
               "Item.Filename", "Item.CorrectResponse",
               "Item.ImageOrder", "CHECK.Trial.ProvideFeedback",
               "Item.Repetitions", "Response",
               "Response.ClickPosition", "Response.ClickPosition.X", "Response.ClickPosition.Y",
               "Time.StartOfStimulus", "Time.EndOfTrial", "Response.RT"),
      sep = ",") %>%
    # Add Experiment information
    mutate(Experiment = experiment) %>%
    { if ("Answer.cond_exp" %in% names(.)) 
      rename(., Condition.Exposure = Answer.cond_exp) else 
        mutate(., Condition.Exposure = NA) } %>%
    rename_at(
      vars(starts_with("Answer.rsrb")), 
      ~ gsub("Answer\\.rsrb\\.([a-z])", "Participant\\.\\U\\1", .x, perl = T)) %>%
    # Make Trial numerical
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(
      # Anonymize workers
      ParticipantID = as.numeric(factor(paste(Assignment.Submit.DateTime.UTC, workerid, AssignmentID))),
      # Extract item information
      REMOVE.Item = gsub("^(.*)\\.(wav)$", "\\1", Item.Filename),
      Item.Height = as.numeric(sapply(strsplit(REMOVE.Item, "_"), function(x) x[2])),
      Item.Backness = as.numeric(sapply(strsplit(REMOVE.Item, "_"), function(x) x[3])),
      Response = factor(Response, levels = levels.response),
      Response.Vowel = factor(
        plyr::mapvalues(Response, levels.response, levels.response.vowel),
        levels = levels.response.vowel)) %>%
    # Variable typing
    mutate_at(vars(CHECK.Trial, ItemID, Item.Repetitions, Response.ClickPosition.X, Response.ClickPosition.Y, Time.StartOfStimulus, Time.EndOfTrial, Response.RT),
              as.numeric) %>%
    mutate_at(vars(ParticipantID, workerid, Phase, Item.Filename, Item.CorrectResponse, Item.ImageOrder,
                   Response, Response.ClickPosition, Trial.ProvideFeedback, CHECK.Trial.ProvideFeedback,
                   hitid, AssignmentID),
              factor) %>%
    mutate(
      Participant.Sex = factor(Participant.Sex, levels.Sex),
      Participant.Race = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Race), 
          c("amerind", "asian", "black", "multiple", "other", "white"),
          c("American Indican", "Asian", "Black", "multiple", "other", "White")), 
        levels.Race),
      Participant.Ethnicity = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Ethnicity),
          c("Hisp", "NonHisp"),
          c("Hispanic", "Non-Hispanic")),
        levels.Ethnicity)) %>%
    # Get durational measures (in minutes)
    group_by(ParticipantID) %>%
    mutate(
      Duration.Assignment = difftime(Assignment.Submit.DateTime.UTC, Assignment.Accept.DateTime.UTC, units = "mins"),
      Duration.AllPhases = (max(Time.EndOfTrial) - min(Time.StartOfStimulus)) / 60000,
      ) %>%
    ungroup() %>%
    # Remove unnecessary columns and order remaining columns
    select(
      -starts_with("CHECK"),
      -starts_with("REMOVE")) %>%
    arrange(Experiment, ParticipantID, Phase, Trial) 
  
  return(.data)
}


sortVars_NORM <- function(.data) {
  .data %>% 
    relocate(
      Experiment,
      starts_with("Experiment."),
      List,
      ParticipantID,
      starts_with("Participant."), 
      starts_with("Condition"), 
      Phase, Trial,
      ItemID,
      starts_with("Item."),
      Response,
      starts_with("Response"),
      starts_with("Duration"),
      starts_with("Time"),
      starts_with("Assignment"),
      starts_with("Answer"),
      everything())
}


prepVars_NORM <- function(.data) {
  contrasts(.data$Item.ImageOrder) <- cbind("f vs. b" = c(-.5, .5))
  contrasts(.data$Experiment) <- cbind("1a (synthesized) vs. 1b (natural)" = c(.5, -.5))
  
  .data %<>% Gelman_scale_cues_NORM
  
  return(.data)
}


addExclusionSummary_NORM <- function(
  d, 
  exclude_based_on_catch_trials = T,
  plot = T, 
  return_data = T
) {
  d %<>%
    mutate(
      Exclude_Participant.Reason = factor(case_when(
        Exclude_Participant.because_of_TechnicalDifficulty ~ "Technical difficulty",
        Exclude_Participant.because_of_MultipleExperiments ~ "Repeat participant",
        Exclude_Participant.because_of_IgnoredInstructions ~ "No headphones",
        Exclude_Participant.because_of_RT ~ "Reaction time",
        Exclude_Participant.because_of_missing_trials ~ "Too many missing trials",
        T ~ "none"
      )))
  
    p <- d %>%
      group_by(Experiment, ParticipantID, Exclude_Participant.Reason, Assignment.Submit.DuringDayTime) %>%
      mutate(Response.log_RT = log10(Response.RT)) %>%
      summarise_at("Response.log_RT", .funs = list("mean" = mean, "sd" = sd), na.rm = T) %>%
      ggplot(aes(x = mean, y = sd)) +
      geom_point(aes(color = Exclude_Participant.Reason, shape = Exclude_Participant.Reason, alpha = Assignment.Submit.DuringDayTime)) +
      geom_rug() +
      scale_x_continuous("mean log-RT (in msec)") +
      scale_y_continuous("SD of log-RT") +
      scale_color_manual(
        "Reason for exclusion",
        breaks = c("none", 
                   "Technical difficulty", "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
        values = c("black", rep("red", 8))) +
      scale_shape_manual(
        "Reason for exclusion",
        breaks = c("none", 
                   "Technical difficulty", "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
        values = c(16, 15, 17, 10, 3, 4, 8, 9, 13)) +
      scale_alpha_manual(
        "Completed during\ndaytime (EST)?",
        breaks = c(T, F),
        values = c(1, .3))
    
  if (plot) { plot(p) }
  if (return_data) return(d) else return(p)
}

# Functions for participant exclusion criteria
add.ExclusionCriteria <- function(
    data,
    # Maximum proportion of trials that can be excluded before ALL of the participant's data is excluded
    max_missing_proportion = 0.2
) {
  data %>%
    group_by(Experiment, ParticipantID) %>%
    # Get mean and SD of log-transformed RTs for each participant
    mutate(
      Response.log_RT = log10(ifelse(Response.RT <= 0, NA_real_, Response.RT)),
      Response.log_RT_scaled = scale(Response.log_RT),
      Response.mean_log_RT = mean(Response.log_RT, na.rm = T),
      # Check whether participants followed the instructions to wear over-ear head-phones and not take the experiment more than once (as responded in the post-experiment survey).
      # Remove all but chronologically first instance of experiments by the same participant
      Exclude_Participant.because_of_MultipleExperiments = ifelse(Assignment.Submit.DateTime.UTC > min(Assignment.Submit.DateTime.UTC), T, F)) %>%
    ungroup() %>%
    mutate(
      Exclude_Participant.because_of_IgnoredInstructions = ifelse(Answer.audio_type %in% c("in-ear", "over-ear"), FALSE, TRUE)) %>%
    # Check participants' log RT; participants who were more than 3 standard deviations faster or slower in their mean (log-transformed) RTs compared to other participants. Also check all trials with RTs more than 3 standard deviations faster or slower than expected
    group_by(Experiment, Trial) %>%
    mutate(
      Exclude_Trial.because_of_RT = c(ifelse(is.na(Response.log_RT_scaled) | abs(scale(Response.log_RT_scaled)) > 3, T, F))) %>%
    ungroup() %>%
    mutate(
      Exclude_Participant.because_of_RT = c(ifelse(abs(scale(Response.mean_log_RT)) > 3, T, F))) %>%
    group_by(ParticipantID) %>%
    mutate(
      Exclude_Participant.because_of_missing_trials = ifelse(sum(Exclude_Trial.because_of_RT | is.na(Response)) / length(Response) > max_missing_proportion, T, F)) %>%
    ungroup() %>% 
    #Exclude participants with unusual response patterns, after qualitative assessment of per participant response plots in SI
    mutate(
      Exclude_Participant.because_of_unusual_vowel_responses = case_when(
        Experiment == "Experiment 1a (natural)" & ParticipantID %in% c(8,13,15,21) ~ TRUE,
        Experiment == "Experiment 1b (synthesized)" & ParticipantID %in% c(15,21) ~ TRUE,
        TRUE ~ FALSE),
      Exclude_Participant.Reason = factor(case_when(
        Exclude_Participant.because_of_MultipleExperiments ~ "Repeat participant",
        Exclude_Participant.because_of_IgnoredInstructions ~ "No headphones",
        Exclude_Participant.because_of_RT ~ "Reaction time",
        Exclude_Participant.because_of_missing_trials ~ "Too many missing trials",
        Exclude_Participant.because_of_unusual_vowel_responses ~ "Unusual vowel responses",
        T ~ "none"))) 
}

excludeData <- function(data) {
  data %<>%
    filter(
      Exclude_Participant.Reason == "none",
      !Exclude_Trial.because_of_RT)
  
  return(data)
}


## Data summaries ------------------------------------------------------- 
getParticipantsPerList_NORM <- function(d) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    arrange(vars(everything()))
}


getMissingParticipantsPerList_NORM <- function(d, targetedParticipantsPerList) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    mutate(n = targetedParticipantsPerList - n) %>%
    filter(n > 0) %>%
    arrange(vars(everything()))
}


plotDemographicInformation_NORM <- function(
  d, 
  rows = NULL,
  cols = NULL
) {
  p.answer <- d %>%
    group_by(Experiment, ParticipantID) %>%
    select(starts_with("Participant."), starts_with("Condition"), "Exclude_Participant.Reason") %>%
    summarize_all(first) %>%
    ggplot(aes(x = Participant.Age, fill = ifelse(Exclude_Participant.Reason == "none", "no", "yes"))) +
    scale_fill_manual("Excluded", values = c("black", "red")) +
    facet_grid(
      cols = vars(!!! syms(cols)), 
      rows = vars(!!! syms(rows)), 
      labeller = label_both)
  
  plot(p.answer + geom_histogram(color = NA, position = position_stack()) + xlab("reported age"))
  plot(p.answer + geom_bar() + aes(x = Participant.Sex) + xlab("reported sex"))
  plot(last_plot() + aes(x = Participant.Ethnicity) + 
         xlab("reported ethnicity") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  plot(last_plot() + aes(x = Participant.Race) +
    xlab("reported race")) 
}

## Model fitting and summarizing ----------------------------------------------------------------
scale_summary <- function(data) { 
  data %>%
    group_by(Experiment, cue_normalization, cue_transform) %>%
    summarise(
      across(
        c(F0, F1, F2, F3, Duration_CCuRE),
        list(mean = mean, sd = sd)))
}


fit_multinomial_baseline_models <- function(data, experiment, re_formula = NA) {
  my_priors <- c(
    # standard deviations of random effects
    prior(cauchy(0, 2.5), class = "sd", dpar = "muheed"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muwhod"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muhood"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muhid"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muhud"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muhead"),
    prior(cauchy(0, 2.5), class = "sd", dpar = "muhad"))
  
  m <- list()
  for (e in experiment) {
    message("Processing experiment ", e)
    data.for_analysis <-
      data %>%
      filter(
        Experiment == .env$e, 
        cue_normalization == "r",
        cue_transform == "Hz") %>%
      select(Experiment, ParticipantID, Response.Vowel, Response) %>%
      { if (e == "Experiment 1a (natural)") 
        mutate(., Response = case_when(
          Response == "odd" ~ "hod",
          Response == "hut" ~ "hud",
          T ~ Response)) else . }
    
    my_formula <- bf(paste("Response ~ 1", if (!is.na(re_formula)) paste("+", re_formula) else  "")) 
    model_name <- paste(e, "INTERCEPTS-ONLY", if (is.na(re_formula)) "noRE" else "RE", sep = "-")
    tic()
    m[[model_name]] <- 
      brm(
        formula = my_formula,
        data = data.for_analysis,
        family = categorical(link = "logit", refcat = "hod"),
        prior = if (!is.na(re_formula)) my_priors else NULL,
        sample_prior = T,
        save_pars = save_pars(all = TRUE),
        control = list(adapt_delta = .95),
        backend = "cmdstanr",
        chains = chains,
        cores = getOption("mc.cores"),
        threads = threading(threads = 4),
        file = paste0("../../models/GLMM-", model_name))
    toc()
  }
  
  return(m)  
}

fit_multinomial_models <- function(data, experiment, cues = c("F1", "F2"), cue_normalization, cue_transform, re_formula = NA, model_type = "GLMM") {
  my_priors <- c()
  if (model_type == "GLMM") {
    my_priors <- append(
      my_priors, 
      # fixed effects
      c(prior(student_t(3, 0, 2.5), class = "b", dpar = "muheed"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muwhod"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muhood"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muhid"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muhud"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muhead"),
        prior(student_t(3, 0, 2.5), class = "b", dpar = "muhad")))
  }
  if (!is.na(re_formula)) {
    my_priors <- append(
      my_priors,
      c(
        # standard deviations of random effects
        prior(cauchy(0, 2.5), class = "sd", dpar = "muheed"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muwhod"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muhood"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muhid"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muhud"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muhead"),
        prior(cauchy(0, 2.5), class = "sd", dpar = "muhad"),
        # correlations of random effects
        prior(lkj(1), class = "cor")))
  }
  
  m <- list()
  
  for (e in experiment) {
    message("Processing experiment ", e)
    for (cn in cue_normalization) {
      message("Processing cue normalization ", cn)
      for (ct in intersect(cue_transform, data %>% 
                           filter(Experiment == .env$e, cue_normalization == .env$cn) %>%
                           distinct(.data$cue_transform) %>% 
                           pull(.data$cue_transform))) {
        message("Preparing model for ", e, " with cues that are in ", ct, " and ", cn, "normalized.")
        scale_info <- 
          scale_summary(data) %>%
          filter(
            Experiment == .env$e, 
            cue_normalization == .env$cn,
            cue_transform == .env$ct)
        
        data.for_analysis <-
          data %>%
          filter(
            Experiment == .env$e, 
            cue_normalization == .env$cn,
            cue_transform == .env$ct) %>%
          select(Experiment, ParticipantID, !!! syms(cues), Response.Vowel, Response) %>%
          mutate(
            across(
              c(!!! syms(cues)),
              ~ (.x - scale_info[[glue("{cur_column()}_mean")]]) / 
                (2 * scale_info[[glue("{cur_column()}_sd")]]))) %>%
          { if (e == "Experiment 1a (natural)") 
            mutate(., Response = case_when(
              Response == "odd" ~ "hod",
              Response == "hut" ~ "hud",
              T ~ Response)) else . }
        
        my_formula <- 
          bf(paste0("Response ~ ",
                    "1 + ", 
                    if (model_type == "GLMM") paste(cues, collapse = " * ") else 
                      paste0("t2(", paste(cues, collapse = ", "), ")"),
                    if (!is.na(re_formula)) {
                      if (model_type == "GLMM") paste0(" + (1 + ", paste(cues, collapse = " * "), " | ParticipantID)") else 
                        stop("REs for GAMMs not yet implemented!")
                    } else ""))
        
        model_name <- paste(model_type, e, paste(cues, collapse = "+"), cn, ct, if (is.na(re_formula)) "noRE" else "RE", sep = "-")
        tic()
        m[[model_name]] <- 
          brm(
            formula = my_formula,
            data = data.for_analysis,
            family = categorical(link = "logit", refcat = "hod"),
            prior = my_priors,
            control = list(adapt_delta = .95, max_treedepth = 15),
            sample_prior = T,
            save_pars = save_pars(all = TRUE),
            backend = "cmdstanr",
            chains = chains,
            cores = getOption("mc.cores"),
            # (parallel::detectCores() - getOption("mc.cores")) %/% 
            #     getOption("mc.cores") * getOption("mc.cores")
            threads = threading(threads = 4),
            file = paste0("../../models/", model_name))
        toc()
      }
    }
  }
  
  return(m)  
}


## Functions for formatting knitr output 
summarize_effect <- function(model, effect, parentheses = T) {
  h <- hypothesis(model, effect)$hypothesis
  op <- gsub("^.*(<|>|=).*$", "\\1", h$Hypothesis)
  
  m <- paste0("$\\hat{\\beta} = ", round(h$Estimate, 3), "; BF_{", op, "0} = ", round(h$Evid.Ratio, 1), "; p_{posterior} = ", round(h$Post.Prob, 2), "$")
  if (parentheses) m <- paste0("(", m, ")")
  
  return(m)
}


subset_grid_to_test_locations <- function(
    data.grid,
    data.test.location,
    multiplier = 1,
    cues = c("F1", "F2")
) {
  require(rlang)
  
  data.test.location %>%
    ungroup() %>%
    distinct(!!! syms(cues))
  
  # First let's get the maximal distance between two neighboring test points
  max.dist <- 
    data.test.location %>%
    cross_join(
      data.test.location %>%
        rename(
          !! sym(paste0(cues[1], "_2")) := !! sym(cues[1]),
          !! sym(paste0(cues[2], "_2")) := !! sym(cues[2]))) %>%
    mutate(dist = sqrt(
      (!! sym(cues[1]) - !! sym(paste0(cues[1], "_2")))^2 + 
        (!! sym(cues[2]) - !! sym(paste0(cues[2], "_2")))^2)) %>%
    group_by(!!! syms(cues)) %>%
    # Remove zero distance for comparing test location against itself
    filter(dist != 0) %>%
    summarise(dist = min(dist)) %>%
    ungroup() %>%
    summarise(dist = max(dist)) %>%
    pull(dist)
  
  data.grid %>%
    left_join(
      data.grid %>%
        ungroup() %>%
        distinct(!!! syms(cues)) %>%
        cross_join(
          data.test.location %>%
            rename(
              !! sym(paste0(cues[1], "_Test")) := !! sym(cues[1]),
              !! sym(paste0(cues[2], "_Test")) := !! sym(cues[2]))) %>%
        mutate(dist = sqrt(
          (!! sym(cues[1]) - !! sym(paste0(cues[1], "_Test")))^2 + 
            (!! sym(cues[2]) - !! sym(paste0(cues[2], "_Test")))^2)) %>%
        group_by(!!! syms(cues)) %>%
        summarise(include = if(min(dist) <= max.dist * multiplier) T else F),
      by = cues) %>%
    filter(include) %>%
    dplyr::select(-include)
}


multilogits2probs <- function(
    data, 
    cues = get_all_cue_names(data), 
    groups = intersect(names(data), c(cues, "ParticipantID", ".draw")),
    refcat = "8"
) {
  require(rlang)
  
  data %>%
    # Add rows for missing (baseline) vowel
    bind_rows(
      data %>%
        distinct(!!! syms(groups), .keep_all = T) %>%
        mutate(
          `logit(vowel | cues)` = 0,
          Response = refcat)) %>%
    group_by(!!! syms(groups)) %>%
    mutate(
      # See https://en.wikipedia.org/wiki/Multinomial_logistic_regression
      # Uncomment for debugging, to check whether each estimate is based on all the log-odds of all vowels:
      # Vowel.n.CHECK = length(Response),
      # s = sum(exp(`logit(vowel | cues)`)),
      `p(vowel | cues)` = exp(`logit(vowel | cues)`) / sum(exp(`logit(vowel | cues)`)),
      # Handle cases of NaN that result from ill-defined division in previous line
      `p(vowel | cues)` = ifelse(is.nan(`p(vowel | cues)`), 
                                 ifelse(
                                   `logit(vowel | cues)` == max(`logit(vowel | cues)`),
                                   1 / sum(`logit(vowel | cues)` == max(`logit(vowel | cues)`)), 
                                   0), 
                                 `p(vowel | cues)`)) %>%
    ungroup()
} 

get_posterior_grid <- function(
    model,
    data = model$data,
    scale_info,
    re_formula = if (str_count(string = model$file, "-noRE") > 0) NA else NULL,
    cues = c("F1", "F2"),
    n.posterior.samples = NULL,
    grid.resolution = 51,
    multiplier = 1,
    batch.size = 1000,
    # set a *random* seed. The purpose of this seed is solely to make sure add_fitted_draws
    # below returns the same draws of the posterior for all slices of the data.
    seed = runif(1)
) {
  # Are random effects included?
  filename <- paste0(gsub("(GLMM|GAMM)", "posterior_samples-\\1", model$file))
  if (str_count(string = model$file, "-noRE")) re_formula <- NA
  filename <- paste0(filename, if (is.null(re_formula)) "-wRandomEffect" else
    if (!is.na(re_formula)) "-wRandomEffect" else "")
  if (file.exists(filename)) return(readRDS(filename))
  
  # Set up parallel processing
  require(tidybayes)
  require(furrr)
  future::plan(multisession, workers = availableCores())
  
  model_pars <- gsub(".rds", "", str_split_1(model$file, "-"))[2:5]
  suppressMessages(
    scale_info %<>% filter(
      Experiment == model_pars[1],
      cue_normalization == model_pars[3],
      cue_transform == model_pars[4]))
  
  if (file.exists(filename)) {
    message("Found old file. Loading it.")
    d.grid <- readRDS(file = filename)
  } else {
    # Get posterior fitted / predicted values
    d.grid <- 
      data %>%
      { if (is.na(re_formula)) {
        modelr::data_grid(
          .,
          !! sym(cues[1]) := seq_range(!! sym(cues[1]), n = grid.resolution, expand = .1),
          !! sym(cues[2]) := seq_range(!! sym(cues[2]), n = grid.resolution, expand = .1)) 
      } else {
        modelr::data_grid(
          .,
          ParticipantID,
          !! sym(cues[1]) := seq_range(!! sym(cues[1]), n = grid.resolution, expand = .1),
          !! sym(cues[2]) := seq_range(!! sym(cues[2]), n = grid.resolution, expand = .1)) }}
    
    # Limit grid to values that aren't too far from the test locations
    suppressMessages(
      d.grid %<>%
        subset_grid_to_test_locations(data.test.location = data, cues = cues, multiplier = multiplier))
    
    model %<>% recover_types(data)
    
    # Now extract posterior for the remaining grid locations
    d.grid %<>%
      split(1:nrow(.) %/% batch.size) %>%
      map_dfr(
        . %>% 
          # Unfortunately transform = T to get predictions in probabilities
          # does not seem to work
          add_linpred_draws(
            object = model, 
            ndraws = n.posterior.samples,
            re_formula = re_formula,
            seed = seed, 
            value = "logit(vowel | cues)",
            category = "Response") %>%
          ungroup() %>%
          dplyr::select(-c(.row, .chain, .iteration)))
    
    message("Completed extraction of predictions. Transforming them.")  
    d.grid %<>%
      split(1:nrow(.) %/% (batch.size * n.posterior.samples)) %>%
      map_dfr(
        . %>% 
          mutate(Response = plyr::mapvalues(Response, 1:7, setdiff(model$family$cats, model$family$refcat)))) %>%
      multilogits2probs(cues = cues, refcat = model$family$refcat)
    
    message("Unscaling Gelman scaled cues, and mapping responses onto vowel categories.")
    d.grid %<>%
          mutate(Response = factor(Response, levels = levels.response.vowel)) %>%
          my_unscale_Gelman(scale_info)
    
    saveRDS(d.grid, file = filename)
  }
  
  future::plan(sequential)
  return(d.grid)
}

get_model_filename <- function(model.type, experiment, cues, cue_normalization, cue_transform, RE)
  paste0(
    paste(
      model.type, 
      experiment, 
      paste(cues, collapse = "+"),
      cue_normalization,
      cue_transform,
      RE,
      sep = "-"),
    ".rds")

make_posterior_prediction_plots <- function(
    experiment,
    cue_normalization,
    cue_transform,
    RE = "noRE",
    scale_info,
    cues = c("F1", "F2"),
    logit = FALSE,
    faceted = FALSE,
    animate.n = NULL,
    model.type = "GLMM"
) {
  require(rlang)

  filename <- get_model_filename(model.type, experiment, cues, cue_normalization, cue_transform, RE)
  model <- readRDS(paste0("../../models/", filename))
  data <- model$data
  
  figure_path <- "../../figures/"
  depvar <- paste0(if (logit) "logit" else "p", "(vowel | cues)")
  groups <- if (!is.null(animate.n)) c(cues, "Response", ".draw") else c(cues, "Response")
  draws <- if (!is.null(animate.n)) sample(unique(data.grid$.draw), animate.n) else NULL
  
  data.grid <- readRDS(paste0("../../models/posterior_samples-", filename))
  data.grid %<>%
    { if (!is.null(animate.n)) filter(., .draw %in% draws) else . } %>% 
    group_by(!!! syms(groups)) %>%
    # I first considered averaging in log-odds and then transforming back to probabilities but
    # marginalization should probably be done at whatever level the predictions are summarized
    summarise(
      !! sym(depvar) := mean(!! sym(depvar)),
      !! sym(paste0(depvar, "sd")) := sd(!! sym(depvar))) %>%
    # Renormalize posterior after averaging
    group_by(!!! syms(setdiff(groups, "Response"))) %>%
    mutate(!! sym(depvar) := !! sym(depvar) / sum(!! sym(depvar))) %>%
    # Reformat response to vowel labels
    mutate(Response = plyr::mapvalues(Response, levels.response, levels.vowel.IPA))
  
  # Determine position of vowel labels
  data.label <- 
    data.grid %>%
    { if (!is.null(animate.n))  group_by(., Response, .draw)  else group_by(., Response) } %>%
    filter(!! sym(depvar) == max(!! sym(depvar))) %>%
    summarise(across(all_of(cues), mean))
  
  suppressMessages(
    scale_info %<>% filter(
      Experiment == .env$experiment,
      cue_normalization == .env$cue_normalization,
      cue_transform == .env$cue_transform))
  shared_plot_components <- 
    list(
      geom_point(
        data = data %>%
          distinct(!!! syms(cues)) %>%
          my_unscale_Gelman(scale_info),
        color = "black", 
        size = .3,
        alpha = 1),
      geom_label(
        data = data.label,
        mapping = aes(label = Response, color = Response),
        fill = "white"),
      scale_x_reverse("F2"),
      scale_y_reverse("F1"),
      scale_alpha_continuous(
        depvar,   
        range = c(0,1)),
      scale_fill_manual(
        "",
        breaks = levels.vowel.IPA,
        labels = levels.vowel.IPA,
        values = colors.vowels),
      scale_color_manual(
        "",
        breaks = levels.vowel.IPA,
        labels = levels.vowel.IPA,
        values = colors.vowels),
      guides(alpha = "none"),
      coord_fixed(expand = FALSE),
      theme(legend.position = "top") 
    )
  
  if (faceted) {
    # By vowel
    p <- 
      data.grid %>%
      ggplot(aes(x = !! sym(cues[2]), y = !! sym(cues[1]))) +
      geom_raster(
        aes(alpha = !! sym(depvar), fill = Response),
        interpolate = T) +
      facet_wrap(~ Response, nrow = 2) +
      shared_plot_components
    
    if (!is.null(animate.n)) {
      p <- p + transition_manual(.draw)
      animate(
        p, 
        renderer = gifski_renderer(
          file = paste0(
            figure_path,
            paste(
              "posterior_predictions_faceted_by_vowel", 
              model.type,
              experiment,
              paste(cues, collapse = "+"),
              cue_normalization,
              cue_transform,
              RE,
              sep = "-"),
            if (logit) "-logit" else "-probability",
            "-animation.gif"),
          width = 1400, height = 400)) 
    } else {
      ggsave(
        p, 
        file = paste0(
          figure_path,
          paste(
            "posterior_predictions_faceted_by_vowel", 
            model.type,
            experiment,
            paste(cues, collapse = "+"),
            cue_normalization,
            cue_transform,
            RE,
            sep = "-"),
          if (logit) "-logit" else "-probability",
          "-marginal.png"),
        width = 8, height = 4) 
    }
  } else {
    p <- data.grid %>%
      ggplot(aes(x = !! sym(cues[2]), y = !! sym(cues[1]))) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[1]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[1],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[2]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[2],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[3]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[3],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[4]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[4],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[5]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[5],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[6]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[6],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[7]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[7],
        interpolate = T) +
      geom_raster(
        data = . %>% filter(Response == levels(data.grid$Response)[8]),
        mapping = aes(alpha = !! sym(depvar)), 
        fill = colors.vowels[8],
        interpolate = T) +
      shared_plot_components
    
    if (!is.null(animate.n)) {
      p <- p + transition_manual(.draw)
      animate(
        p, 
        renderer = gifski_renderer(
          file = paste0(
            figure_path,
            paste(
              "posterior_predictions", 
              model.type,
              experiment,
              paste(cues, collapse = "+"),
              cue_normalization,
              cue_transform,
              RE,
              sep = "-"),
            if (logit) "-logit" else "-probability",
            "-animation.gif"),
          width = 800, height = 800)) 
    } else {
      ggsave(
        p, 
        file = paste0(
          figure_path,
          paste(
            "posterior_predictions_faceted_by_vowel", 
            model.type,
            experiment,
            paste(cues, collapse = "+"),
            cue_normalization,
            cue_transform,
            RE,
            sep = "-"),
          if (logit) "-logit" else "-probability",
          "-marginal.png"),
        width = 8, height = 8) 
    }
  }
  
  if (is.null(animate.n)) return (p) 
}