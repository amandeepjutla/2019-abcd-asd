# my_ggcoef.r
# 20191002 (updated 20191204)
# Crudely hacked-up version of the ggcoef function from ggstatsplot 0.1.2 that 
# allows a series of arbitrary colors for labels, passed via
# a character vector of hex codes called `colors_to_use`

#' @title Dot-and-whisker plots for regression analyses
#' @name ggcoefstats
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers and other statistical details included as
#'   labels.
#'
#' @param x A model object to be tidied with `broom::tidy`, or a tidy data frame
#'   containing results. If a data frame is to be plotted, it *must* contain
#'   columns named `term` (names of predictors), or `estimate` (corresponding
#'   estimates of coefficients or other quantities of interest). Other optional
#'   columns are `conf.low` and `conf.high` (for confidence intervals);
#'   `p.value`. It is important that all `term` names should be unique.
#' @param output,return Character describing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results from `broom::tidy`) or `"glance"` (object from
#'   `broom::glance`) or `"augment"` (object from `broom::augment`).
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"`) in the label. This is especially important if the `x` argument in
#'   `ggcoefstats` is a dataframe in which case the function wouldn't know what
#'   kind of model it is dealing with.
#' @param bf.message Logical that decides whether results from running a
#'   Bayesian meta-analysis assuming that the effect size *d* varies across
#'   studies with standard deviation *t* (i.e., a random-effects analysis)
#'   should be displayed in caption. Defaults to `TRUE`.
#' @param xlab Label for `x` axis variable (Default: `"regression coefficient"`).
#' @param ylab Label for `y` axis variable (Default: `"term"`).
#' @param subtitle The text for the plot subtitle. The input to this argument
#'   will be ignored if `meta.analytic.effect` is set to `TRUE`.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`. Default is no correction
#'   (`"none"`). This argument is relevant for multiplicity correction for
#'   multiway ANOVA designs (see,
#'   \href{https://link.springer.com/article/10.3758/s13423-015-0913-5}{Cramer
#'   et al., 2015}).
#' @param point.color Character describing color for the point (Default:
#'   `"blue"`).
#' @param point.size Numeric specifying size for the point (Default: `3`).
#' @param point.shape Numeric specifying shape to draw the points (Default: `16`
#'   (**a dot**)).
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence intervals (Default:
#'   `0.95`). For MCMC model objects (Stan, JAGS, etc.), this will be
#'   probability level for CI.
#' @param coefficient.type Relevant only for ordinal regression models (`clm` ,
#'   `clmm`, `"svyolr"`, and `polr`), this argument decides which parameters are
#'   display in the plot. Available parameters are: parameter that measures the
#'   **intercept**, i.e. the log-odds distance between response values
#'   (`"alpha"`); effects on the **location** (`"beta"`); or effects on the
#'   **scale** (`"zeta"`). For `clm` and `clmm` models, by default, only
#'   `"beta"` (a vector of regression parameters) parameters will be show. Other
#'   options are `"alpha"` (a vector of threshold parameters) or `"both"`. For
#'   `polr` models, by default, only `"coefficient"` will be shown. Other option
#'   is to show `"zeta"` parameters. Note that, from `broom 0.7.0` onward,
#'   coefficients will be renamed and `"intercept"` type coefficients will
#'   correspond to `"alpha"` parameters, `"location"` type coefficients will
#'   correspond to `"beta"` parameters, and `"scale"` type coefficients will
#'   correspond to `"zeta"` parameters.
#' @param by.class A logical indicating whether or not to show performance
#'   measures broken down by class. Defaults to `FALSE`. When `by.class = FALSE`
#'   only returns a tibble with accuracy and kappa statistics. Mostly relevant
#'   for an object of class `"confusionMatrix"`.
#' @param se.type Character specifying the method used to compute standard
#'   standard errors for quantile regression (Default: `"nid"`). To see all
#'   available methods, see `quantreg::summary.rq()`.
#' @param nboot Number of bootstrap samples for confidence intervals for partial
#'   eta-squared and omega-squared (Default: `500`). This argument is relevant
#'   only for models objects of class `aov`, `anova`, and `aovlist`.
#' @param effsize Character describing the effect size to be displayed: `"eta"`
#'   (default) or `"omega"`. This argument is relevant
#'   only for models objects of class `aov`, `anova`, and `aovlist`.
#' @param partial Logical that decides if partial eta-squared or omega-squared
#'   are returned (Default: `TRUE`). If `FALSE`, eta-squared or omega-squared
#'   will be returned. Valid only for objects of class `aov`, `anova`, or
#'   `aovlist`.
#' @param meta.analytic.effect Logical that decides whether subtitle for
#'   meta-analysis via linear (mixed-effects) models - as implemented in the
#'   `metafor` package - is to be displayed (default: `FALSE`). If `TRUE`, input
#'   to argument `subtitle` will be ignored. This will be mostly relevant if a
#'   data frame with estimates and their standard errors is entered as input to
#'   `x` argument.
#' @param k Number of decimal places expected for results displayed in labels
#'   (Default : `k = 2`).
#' @param k.caption.summary Number of decimal places expected for results
#'   displayed in captions (Default : `k.caption.summary = 0`).
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `TRUE`).
#' @param exponentiate If `TRUE`, the `x`-axis will be logarithmic (Default:
#'   `FALSE`).
#' @param errorbar.color Character deciding color of the error bars (Default:
#'   `"black"`).
#' @param errorbar.height Numeric specifying the height of the error bars
#'   (Default: `0`).
#' @param errorbar.linetype Line type of the error bars (Default: `"solid"`).
#' @param errorbar.size Numeric specifying the size of the error bars (Default:
#'   `0.5`).
#' @param vline Decides whether to display a vertical line (Default: `"TRUE"`).
#' @param vline.color Character specifying color of the vertical line (Default:
#'   `"black"`).
#' @param vline.linetype Character specifying line type of the vertical line
#'   (Default: `"dashed"`).
#' @param vline.size Numeric specifying the size of the vertical line (Default:
#'   `1`).
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and p-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel` (Default: `TRUE`).
#' @param caption.summary Logical. Decides whether the model summary should be
#'   displayed as a cation to the plot (Default: `TRUE`). Color of the line
#'   segment. Defaults to the same color as the text.
#' @param stats.label.size,stats.label.fontface,stats.label.color Aesthetics for
#'   the labels. Defaults: `3`, `"bold"`,`NULL`, resp. If `stats.label.color` is
#'   `NULL`, colors will be chosen from the specified `package` (Default:
#'   `"RColorBrewer"`) and `palette` (Default: `"Dark2"`).
#' @param label.r, Radius of rounded corners, as unit or number. Defaults to
#'   `0.15`. (Default unit is lines).
#' @param label.size Size of label border, in mm. Defaults to `0.25`.
#' @param label.box.padding	 Amount of padding around bounding box, as number.
#'   Defaults to `1`. (Default unit is lines).
#' @param label.label.padding	 Amount of padding around label, as number.
#'   Defaults to `0.25`. (Default unit is lines).
#' @param label.point.padding	 Amount of padding around labeled point, as
#'   number. Defaults to `0`. (Default unit is lines).
#' @param label.segment.color Color of the line segment (Default: `"grey50"`).
#' @param label.segment.size Width of line segment connecting the data point to
#'   the text label, in mm. Defaults to `0.5`.
#' @param label.segment.alpha Transparency of the line segment. Defaults to the
#'   same transparency as the text.
#' @param label.min.segment.length Skip drawing segments shorter than this.
#'   Defaults to `0.5`. (Default unit is lines).
#' @param label.force Force of repulsion between overlapping text labels.
#'   Defaults to `1`.
#' @param label.max.iter Maximum number of iterations to try to resolve
#'   overlaps. Defaults to `2000`.
#' @param label.nudge.x,label.nudge.y Horizontal and vertical adjustments to
#'   nudge the starting position of each text label. Defaults to `0`.
#' @param label.xlim,label.ylim Limits for the `x` and `y` axes. Text labels
#'   will be constrained to these limits. By default, text labels are
#'   constrained to the entire plot area. Defaults to `c(NA, NA)`.
#' @param label.direction Character (`"both"`, `"x"`, or `"y"`) -- direction in
#'   which to adjust position of labels (Default: `"y"`).
#' @param ... Additional arguments to tidying method.
#' @inheritParams bf_meta_message
#' @inheritParams broom.mixed::tidy.merMod
#' @inheritParams broom::tidy.clm
#' @inheritParams broom::tidy.polr
#' @inheritParams broom::tidy.mjoint
#' @inheritParams theme_ggstatsplot
#' @inheritParams paletteer::paletteer_d
#' @inheritParams subtitle_meta_ggcoefstats
#' @inheritParams ggbetweenstats
#'
#' @import ggplot2
#' @importFrom broomExtra tidy glance augment
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if n
#' @importFrom dplyr group_by arrange full_join vars matches desc everything
#' @importFrom dplyr vars all_vars filter_at starts_with row_number
#' @importFrom stats as.formula lm confint qnorm p.adjust
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom parameters p_value
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom tidyr unite
#' @importFrom groupedstats lm_effsize_standardizer
#' @importFrom insight is_model
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # -------------- with model object --------------------------------------
#'
#' # model object
#' mod <- lm(formula = mpg ~ cyl * am, data = mtcars)
#'
#' # to get a plot
#' ggstatsplot::ggcoefstats(x = mod, output = "plot")
#'
#' # to get a tidy dataframe
#' ggstatsplot::ggcoefstats(x = mod, output = "tidy")
#'
#' # to get a glance summary
#' ggstatsplot::ggcoefstats(x = mod, output = "glance")
#'
#' # to get augmented dataframe
#' ggstatsplot::ggcoefstats(x = mod, output = "augment")
#'
#' # -------------- with custom dataframe -----------------------------------
#'
#' # creating a dataframe
#' df <-
#'   structure(
#'     list(
#'       term = structure(
#'         c(3L, 4L, 1L, 2L, 5L),
#'         .Label = c(
#'           "Africa",
#'           "Americas", "Asia", "Europe", "Oceania"
#'         ),
#'         class = "factor"
#'       ),
#'       estimate = c(
#'         0.382047603321706,
#'         0.780783111514665,
#'         0.425607573765058,
#'         0.558365541235078,
#'         0.956473848429961
#'       ),
#'       std.error = c(
#'         0.0465576338644502,
#'         0.0330218199731529,
#'         0.0362834986178494,
#'         0.0480571500648261,
#'         0.062215818388157
#'       ),
#'       statistic = c(
#'         8.20590677855356,
#'         23.6444603038067,
#'         11.7300588415607,
#'         11.6187818146078,
#'         15.3734833553524
#'       ),
#'       conf.low = c(
#'         0.290515146096969,
#'         0.715841986960399,
#'         0.354354575031406,
#'         0.46379116008131,
#'         0.827446138277154
#'       ),
#'       conf.high = c(
#'         0.473580060546444,
#'         0.845724236068931,
#'         0.496860572498711,
#'         0.652939922388847,
#'         1.08550155858277
#'       ),
#'       p.value = c(
#'         3.28679518728519e-15,
#'         4.04778497135963e-75,
#'         7.59757330804449e-29,
#'         5.45155840151592e-26,
#'         2.99171217913312e-13
#'       ),
#'       df.residual = c(
#'         394L, 358L, 622L,
#'         298L, 22L
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c(
#'       "tbl_df",
#'       "tbl", "data.frame"
#'     )
#'   )
#'
#' # plotting the dataframe
#' ggstatsplot::ggcoefstats(
#'   x = df,
#'   statistic = "t",
#'   meta.analytic.effect = TRUE,
#'   k = 3
#' )
#' }
#' # -------------- getting model summary ------------------------------
#'
#' # model
#' library(lme4)
#' lmm1 <- lme4::lmer(
#'   formula = Reaction ~ Days + (Days | Subject),
#'   data = sleepstudy
#' )
#'
#' # dataframe with model summary
#' ggstatsplot::ggcoefstats(x = lmm1, output = "glance")
#'
#' # -------------- getting augmented dataframe ------------------------------
#'
#' # setup
#' set.seed(123)
#' library(survival)
#'
#' # fit
#' cfit <-
#'   survival::coxph(formula = Surv(time, status) ~ age + sex, data = lung)
#'
#' # augmented dataframe
#' ggstatsplot::ggcoefstats(
#'   x = cfit,
#'   data = lung,
#'   output = "augment",
#'   type.predict = "risk"
#' )
#' @export

# function body
my_ggcoefstats <- function(x,
                        output = "plot",
                        statistic = NULL,
                        scales = NULL,
                        component = "survival",
                        bf.message = TRUE,
                        d = "norm",
                        d.par = c(mean = 0, sd = 0.3),
                        tau = "halfcauchy",
                        tau.par = c(scale = 0.5),
                        iter = 5000,
                        summarize = "stan",
                        p.adjust.method = "none",
                        coefficient.type = c("beta", "location", "coefficient"),
                        by.class = FALSE,
                        effsize = "eta",
                        partial = TRUE,
                        nboot = 500,
                        meta.analytic.effect = FALSE,
                        point.color = "blue",
                        point.size = 3,
                        point.shape = 16,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        se.type = "nid",
                        k = 2,
                        k.caption.summary = 0,
                        exclude.intercept = TRUE,
                        exponentiate = FALSE,
                        errorbar.color = "black",
                        errorbar.height = 0,
                        errorbar.linetype = "solid",
                        errorbar.size = 0.5,
                        vline = TRUE,
                        vline.color = "black",
                        vline.linetype = "dashed",
                        vline.size = 1,
                        sort = "none",
                        xlab = "regression coefficient",
                        ylab = "term",
                        title = NULL,
                        subtitle = NULL,
                        stats.labels = TRUE,
                        caption = NULL,
                        caption.summary = TRUE,
                        stats.label.size = 3,
                        stats.label.fontface = "bold",
                        stats.label.color = NULL,
                        label.r = 0.15,
                        label.size = 0.25,
                        label.box.padding = 1,
                        label.label.padding = 0.25,
                        label.point.padding = 0.5,
                        label.segment.color = "grey50",
                        label.segment.size = 0.5,
                        label.segment.alpha = NULL,
                        label.min.segment.length = 0.5,
                        label.force = 1,
                        label.max.iter = 2000,
                        label.nudge.x = 0,
                        label.nudge.y = 0,
                        label.xlim = c(NA, NA),
                        label.ylim = c(NA, NA),
                        label.direction = "y",
                        package = "RColorBrewer",
                        palette = "Dark2",
                        direction = 1,
                        ggtheme = ggplot2::theme_bw(),
                        ggstatsplot.layer = TRUE,
                        messages = FALSE,
                        colors_to_use = NULL,
                        return = NULL,
                        ...) {
  output <- return %||% output

  # =================== list of objects (for tidy and glance) ================

  # creating a list of objects which will have fixed and random "effects"
  # only fixed effects will be selected
  mixed.mods <-
    c(
      "bglmerMod",
      "blmerMod",
      "brmsfit",
      "brmsfit_multiple",
      "gamlss",
      "glmmadmb",
      "glmerMod",
      "glmmPQL",
      "glmmTMB",
      "gls",
      "lme",
      "lmerMod",
      "mcmc",
      "MCMCglmm",
      "merMod",
      "nlmerMod",
      "rjags",
      "rlmerMod",
      "stanfit",
      "stanreg",
      "stanmvreg",
      "TMB",
      "wblm"
    )

  # =================== types of models =====================================

  # models for which statistic is F-value
  f.mods <-
    c(
      "aov",
      "aovlist",
      "anova",
      "Gam",
      "manova"
    )

  # ============================= model summary ============================

  # creating glance dataframe
  glance_df <- broomExtra::glance(x)

  # if the object is not a dataframe, check if summary caption is to be displayed
  if (isTRUE(insight::is_model(x))) {
    # if glance is not available, inform the user
    if (is.null(glance_df) || !all(c("logLik", "AIC", "BIC") %in% names(glance_df))) {
      # inform the user
      message(cat(
        crayon::green("Note: "),
        crayon::blue("No model diagnostics information available, so skipping caption.\n"),
        sep = ""
      ))

      # and skip the caption
      caption.summary <- FALSE
    }
  }

  # ============================= dataframe ===============================

  if (isFALSE(insight::is_model(x))) {
    # set tidy_df to entered dataframe
    tidy_df <- tibble::as_tibble(x)

    # check that `statistic` is specified
    if (rlang::is_null(statistic)) {
      # skip labels
      stats.labels <- FALSE

      # inform the user
      if (output == "plot") {
        message(cat(
          crayon::red("Note"),
          crayon::blue(": For the object of class"),
          crayon::yellow(class(x)[[1]]),
          crayon::blue(", the argument `statistic` must be specified ('t', 'z', or 'f').\n"),
          crayon::blue("Statistical labels will therefore be skipped.\n"),
          sep = ""
        ))
      }
    }
  }

  # =========================== broom.mixed tidiers =======================

  if (isTRUE(insight::is_model(x))) {
    if (class(x)[[1]] %in% mixed.mods) {
      # getting tidy output using `broom.mixed`
      tidy_df <-
        broomExtra::tidy(
          x = x,
          conf.int = conf.int,
          # exponentiate = exponentiate,
          conf.level = conf.level,
          effects = "fixed",
          scales = scales,
          ...
        )

      # ====================== tidying F-statistic objects ===================
    } else if (class(x)[[1]] %in% f.mods) {
      # creating dataframe
      tidy_df <-
        groupedstats::lm_effsize_standardizer(
          object = x,
          effsize = effsize,
          partial = partial,
          conf.level = conf.level,
          nboot = nboot
        ) %>%
        dplyr::rename(.data = ., statistic = F.value)

      # prefix for effect size
      if (isTRUE(partial)) {
        effsize.prefix <- "partial"
      } else {
        effsize.prefix <- NULL
      }

      # renaming the `xlab` according to the estimate chosen
      xlab <- paste(effsize.prefix, " ", effsize, "-squared", sep = "")

      # ==================== tidying everything else ===========================
    } else {
      tidy_df <-
        broomExtra::tidy(
          x = x,
          conf.int = conf.int,
          conf.level = conf.level,
          se.type = se.type,
          by_class = by.class,
          component = component,
          # exponentiate = exponentiate,
          parametric = TRUE, # relevant for `gam` objects
          ...
        )
    }
  }

  # =================== tidy dataframe cleanup ================================

  # check for the one necessary column
  if (rlang::is_null(tidy_df) || !"estimate" %in% names(tidy_df)) {
    stop(message(cat(
      crayon::red("Error: "),
      crayon::blue("The object of class "),
      crayon::yellow(class(x)[[1]]),
      crayon::blue(" *must* contain column called 'estimate' in tidy output.\n"),
      crayon::blue("Check the tidy output using `broomExtra::tidy(x)`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

  # create a new term column if it's not present
  if (!"term" %in% names(tidy_df)) {
    tidy_df %<>%
      dplyr::mutate(.data = ., term = dplyr::row_number()) %>%
      dplyr::mutate(.data = ., term = paste("term", term, sep = "_"))
  }

  # selecting needed coefficients/parameters for ordinal regression models
  if (any(names(tidy_df) %in% c("coefficient_type", "coef.type"))) {
    if (any(coefficient.type %in%
      c("alpha", "beta", "zeta", "intercept", "location", "scale", "coefficient"))) {
      # subset the dataframe, only if not all coefficients are to be retained
      tidy_df %<>%
        dplyr::filter_at(
          .tbl = .,
          .vars = dplyr::vars(dplyr::starts_with("coef")),
          .vars_predicate = dplyr::all_vars(. %in% coefficient.type)
        )
    }
  }

  # changing names of columns to the required format for `aareg` objects
  if (class(x)[[1]] == "aareg") {
    tidy_df %<>%
      dplyr::rename(
        .data = .,
        coefficient = statistic,
        statistic = statistic.z
      )
  }

  # =================== check for duplicate terms ============================

  # for some class of objects, there are going to be duplicate terms
  # create a new column by collapsing original `variable` and `term` columns
  if (class(x)[[1]] %in% c("gmm", "lmodel2", "gamlss", "drc", "mlm")) {
    tidy_df %<>%
      tidyr::unite(
        data = .,
        col = "term",
        dplyr::matches("term|variable|parameter|method|curveid|curve|response"),
        remove = TRUE,
        sep = "_"
      )
  }

  # halt if there are repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    message(cat(
      crayon::red("Error: "),
      crayon::blue("All elements in the column `term` should be unique.\n"),
      sep = ""
    ))
    return(invisible(tidy_df))
  }

  # =================== p-value computation ==================================

  # p-values won't be computed by default for some of the models
  if (isTRUE(insight::is_model(x)) && !"p.value" %in% names(tidy_df)) {
    # use `sjstats` S3 methods to add them to the tidy dataframe
    tryCatch(
      expr = tidy_df %<>%
        dplyr::full_join(
          x = dplyr::mutate_at(
            .tbl = .,
            .vars = "term",
            .funs = ~ as.character(x = .)
          ),
          y = parameters::p_value(model = x, method = "wald", component = "all") %>%
            dplyr::rename(.data = ., term = Parameter, p.value = p) %>%
            dplyr::mutate_at(
              .tbl = .,
              .vars = "term",
              .funs = ~ as.character(x = .)
            ),
          by = "term"
        ) %>%
        dplyr::filter(.data = ., !is.na(estimate)) %>%
        tibble::as_tibble(x = .),
      error = function(e) tidy_df
    )
  }

  # ================== statistic and p-value check ===========================

  # if broom output doesn't contain p-value or statistic column
  if (sum(c("p.value", "statistic") %in% names(tidy_df)) != 2) {
    # skip the labels
    stats.labels <- FALSE

    # inform the user that skipping labels for the same reason
    # (relevant only in case of a plot)
    if (output == "plot") {
      message(cat(
        crayon::green("Note: "),
        crayon::blue("No p-values and/or statistic available for the model object;"),
        crayon::blue("\nskipping labels with stats.\n"),
        sep = ""
      ))
    }
  }

  # ==================== confidence intervals check ===========================

  # if broom output doesn't contain CI
  if (!"conf.low" %in% names(tidy_df)) {

    # if standard error is present, create confidence intervals
    if ("std.error" %in% names(tidy_df)) {
      # probability for computing confidence intervals
      prob <- 1 - ((1 - conf.level) / 2)

      # computing confidence intervals
      tidy_df %<>%
        dplyr::mutate(
          .data = .,
          conf.low = estimate - stats::qnorm(prob) * std.error,
          conf.high = estimate + stats::qnorm(prob) * std.error
        )
    } else {
      # add NAs so that only dots will be shown
      tidy_df %<>%
        dplyr::mutate(.data = ., conf.low = NA_character_, conf.high = NA_character_)

      # stop displaying whiskers
      conf.int <- FALSE

      # inform the user that skipping labels for the same reason
      message(cat(
        crayon::green("Note: "),
        crayon::blue("No confidence intervals available for regression coefficients"),
        crayon::blue("object, so skipping whiskers in the plot.\n"),
        sep = ""
      ))
    }
  }

  # ============= intercept, exponentiation, and final tidy dataframe =========

  # ordering the dataframe
  tidy_df %<>%
    dplyr::select(
      .data = .,
      term,
      estimate,
      conf.low,
      conf.high,
      dplyr::everything()
    )

  # whether to show model intercept
  # if not, remove the corresponding terms from the dataframe
  if (isTRUE(exclude.intercept)) {
    tidy_df %<>%
      dplyr::filter(
        .data = .,
        !grepl(pattern = "(Intercept)", x = term, ignore.case = TRUE)
      )
  }

  # if the coefficients are to be exponentiated, the label positions will also
  # have to be adjusted
  if (isTRUE(exponentiate)) {
    tidy_df %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = dplyr::vars(dplyr::matches(match = "estimate|conf", ignore.case = TRUE)),
        .funs = ~ exp(x = .)
      )
  }

  # ========================== p-value adjustment ===========================

  # clean up the p-value column
  if ("p.value" %in% names(tidy_df)) {
    # if p-value column is not numeric
    if (!purrr::is_bare_numeric(tidy_df$p.value)) {
      tidy_df %<>%
        dplyr::mutate(.data = ., p.value = as.numeric(as.character(p.value)))
    }

    # adjust the p-values based on the adjustment used
    tidy_df %<>%
      dplyr::mutate(
        .data = ., p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
      )
  }

  # ========================== preparing label ================================

  # adding a column with labels to be used with `ggrepel`
  if (isTRUE(stats.labels)) {
    # in case a dataframe was entered, `x` and `tidy_df` are going to be same
    if (isFALSE(insight::is_model(x))) {
      x <- tidy_df
    }

    # adding a column with labels using custom function
    tidy_df %<>%
      ggcoefstats_label_maker(
        x = x,
        statistic = statistic,
        tidy_df = .,
        glance_df = glance_df,
        k = k,
        effsize = effsize,
        partial = partial
      )
  }

  # ============== meta-analysis plus Bayes factor =========================

  # check if meta-analysis is to be run
  if (isTRUE(meta.analytic.effect) && "std.error" %in% names(tidy_df)) {
    if (dim(dplyr::filter(.data = tidy_df, is.na(std.error)))[[1]] > 0L) {
      # inform the user that skipping labels for the same reason
      message(cat(
        crayon::red("Error: "),
        crayon::blue("At least one of the `std.error` column values is `NA`.\n"),
        crayon::blue("No meta-analysis will be carried out.\n"),
        sep = ""
      ))

      # turn off meta-analysis
      meta.analytic.effect <- FALSE
    }
  }

  # running meta-analysis
  if (isTRUE(meta.analytic.effect)) {
    # results from frequentist random-effects meta-analysis
    subtitle <-
      subtitle_meta_ggcoefstats(
        data = tidy_df,
        k = k,
        messages = messages,
        output = "subtitle"
      )

    # results from Bayesian random-effects meta-analysis
    if (isTRUE(bf.message)) {
      caption <-
        bf_meta_message(
          caption = caption,
          data = tidy_df,
          k = k,
          messages = messages,
          d = d,
          d.par = d.par,
          tau = tau,
          tau.par = tau.par,
          iter = iter,
          summarize = summarize
        )
    }

    # model summary
    caption.meta <-
      subtitle_meta_ggcoefstats(
        data = tidy_df,
        k = k,
        caption = caption,
        messages = FALSE,
        output = "caption"
      )
  }

  # ========================== summary caption ================================

  # caption containing model diagnostics
  if (isTRUE(caption.summary)) {
    # for dataframe objects
    if (isFALSE(insight::is_model(x)) && isTRUE(meta.analytic.effect)) {
      caption <- caption.meta
    }

    # for non-dataframe objects
    if (isTRUE(insight::is_model(x)) && !is.na(glance_df$AIC[[1]])) {
      # preparing caption with model diagnostics
      caption <-
        substitute(
          atop(displaystyle(top.text),
            expr =
              paste(
                "AIC = ",
                AIC,
                ", BIC = ",
                BIC,
                ", log-likelihood = ",
                LL
              )
          ),
          env = list(
            top.text = caption,
            AIC = specify_decimal_p(x = glance_df$AIC[[1]], k = k.caption.summary),
            BIC = specify_decimal_p(x = glance_df$BIC[[1]], k = k.caption.summary),
            LL = specify_decimal_p(x = glance_df$logLik[[1]], k = k.caption.summary)
          )
        )
    }
  }

  # ========================== sorting ===================================

  # whether the term need to be arranged in any specified order
  tidy_df %<>%
    dplyr::mutate(.data = ., term = as.factor(term)) %>%
    tibble::rownames_to_column(.data = ., var = "rowid")

  # sorting factor levels
  new_order <-
    switch(
      sort,
      "none" = order(tidy_df$rowid, decreasing = FALSE),
      "ascending" = order(tidy_df$estimate, decreasing = FALSE),
      "descending" = order(tidy_df$estimate, decreasing = TRUE),
      order(tidy_df$rowid, decreasing = FALSE)
    )

  # sorting `term` factor levels according to new sorting order
  tidy_df %<>%
    dplyr::mutate(.data = ., term = as.character(term)) %>%
    dplyr::mutate(.data = ., term = factor(x = term, levels = term[new_order])) %>%
    dplyr::select(.data = ., -rowid)

  # ========================== basic plot ===================================

  # palette check is necessary only if output is a plot
  if (output == "plot") {
    # setting up the basic architecture
    plot <-
      ggplot2::ggplot(data = tidy_df, mapping = ggplot2::aes(x = estimate, y = term))

    # if needed, adding the vertical line
    if (isTRUE(vline)) {
      # either at 1 - if coefficients are to be exponentiated - or at 0
      if (isTRUE(exponentiate)) {
        xintercept <- 1
      } else {
        xintercept <- 0
      }

      # adding the line geom
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = xintercept,
          color = vline.color,
          linetype = vline.linetype,
          size = vline.size,
          na.rm = TRUE
        )

      # logarithmic scale for exponent of coefficients
      if (isTRUE(exponentiate)) {
        plot <- plot + ggplot2::scale_x_log10()
      }
    }

    # if the confidence intervals are to be displayed on the plot
    if (isTRUE(conf.int)) {
      plot <- plot +
        ggplot2::geom_errorbarh(
          ggplot2::aes_string(xmin = "conf.low", xmax = "conf.high"),
          color = errorbar.color,
          height = errorbar.height,
          linetype = errorbar.linetype,
          size = errorbar.size,
          na.rm = TRUE
        )
    }

    # changing the point aesthetics
    plot <- plot +
      ggplot2::geom_point(
        color = point.color,
        size = point.size,
        shape = point.shape,
        na.rm = TRUE
      )

    # ========================= ggrepel labels ================================

    # adding the labels
    if (isTRUE(stats.labels)) {
      # removing all rows that have NAs anywhere in the columns of interest
      tidy_df %<>%
        dplyr::filter_at(
          .tbl = .,
          .vars = dplyr::vars(dplyr::matches("estimate|statistic|std.error|p.value")),
          .vars_predicate = dplyr::all_vars(!is.na(.))
        )

      # ========================== palette check =================================

      # counting the number of terms in the tidy dataframe
      count_term <- length(tidy_df$term)

      # if no. of factor levels is greater than the default palette color count
 #     palette_message(
 #       package = package,
 #       palette = palette,
 #       min_length = count_term
 #     )

      # computing the number of colors in a given palette
      palette_df <-
        tibble::as_tibble(x = paletteer::palettes_d_names) %>%
        dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
        dplyr::select(.data = ., length)

      # if insufficient number of colors are available in a given palette
#      if (palette_df$length[[1]] < count_term) stats.label.color <- "black"

      # if user has not specified colors, then use a color palette
#      if (model_used==1) {
#          stats.label.color <- c(
#            "#304890", 
#            "#304890",
#            "#304890", 
#            "#304890",
#            "#304890", 
#            "#90A8C0",
#            "#90A8C0", 
#            "#304890",
#            "#304890", 
#            "#90A8C0",
#            "#304890")
#      }

      stats.label.color <- colors_to_use



      # adding labels
      plot <- plot +
        ggrepel::geom_text_repel(
          data = tidy_df,
          mapping = ggplot2::aes(x = estimate, y = term, label = label),
          size = stats.label.size,
          fontface = stats.label.fontface,
          color = stats.label.color,
          box.padding = grid::unit(x = label.box.padding, units = "lines"),
#          label.padding = grid::unit(x = label.label.padding, units = "lines"),
          point.padding = grid::unit(x = label.point.padding, units = "lines"),
#          label.r = grid::unit(x = label.r, units = "lines"),
#          label.size = label.size,
          segment.color = label.segment.color,
          segment.size = label.segment.size,
          segment.alpha = label.segment.alpha,
          min.segment.length = label.min.segment.length,
          force = label.force,
          max.iter = label.max.iter,
          nudge_x = label.nudge.x,
          nudge_y = label.nudge.y,
          xlim = label.xlim,
          ylim = label.ylim,
          na.rm = TRUE,
          show.legend = FALSE,
          direction = label.direction,
          parse = TRUE,
          seed = 123
        )
    }

    # ========================== annotations =============================

    # adding other labels to the plot
    plot <- plot +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        caption = caption,
        subtitle = subtitle,
        title = title
      ) +
      ggstatsplot::theme_mprl(
        ggtheme = ggtheme,
        ggstatsplot.layer = ggstatsplot.layer
      ) +
      ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))
  }

  # =========================== output =====================================

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "plot" = plot,
    "tidy" = tidy_df,
    "dataframe" = tidy_df,
    "df" = tidy_df,
    "glance" = glance_df,
    "summary" = glance_df,
    "augment" = tibble::as_tibble(broomExtra::augment(x = x, ...)),
    "plot"
  ))
}

#' @title Display normality test result as a message.
#' @name normality_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param x A numeric vector.
#' @param lab A character describing label for the variable. If `NULL`, a
#'   generic `"x"` label will be used.
#' @param output What output is desired: `"message"` (default) or `"stats"` (or
#'   `"tidy"`) objects.
#' @param ... Additional arguments (ignored).
#' @inheritParams ggbetweenstats
#'
#' @importFrom stats shapiro.test
#' @importFrom crayon green blue yellow red
#'
#' @inherit stats::shapiro.test return value
#'
#' @family helper_messages
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @examples
#'
#' # message
#' normality_message(
#'   x = anscombe$x1,
#'   lab = "x1",
#'   k = 3
#' )
#'
#' # statistical test object
#' ggstatsplot::normality_message(
#'   x = anscombe$x2,
#'   output = "tidy"
#' )
#' @export

# function body
normality_message <- function(x,
                              lab = NULL,
                              k = 2,
                              output = "message",
                              ...) {

  # if label is not provided, use generic "x" variable
  if (is.null(lab)) lab <- "x"

  # works only if sample size is greater than 3 and less than 5000
  if (length(x) > 3 && length(x) < 5000) {
    # test object
    sw_norm <- stats::shapiro.test(x)
    p_value <- sw_norm$p.value[[1]]

    # what object to return?
    if (output == "message") {
      # exact message
      message(cat(
        crayon::green("Note: "),
        crayon::blue("Shapiro-Wilk Normality Test for "),
        crayon::yellow(lab),
        crayon::blue(": p-value = "),
        crayon::yellow(specify_decimal_p(x = p_value, k = k, p.value = TRUE)),
        "\n",
        sep = ""
      ))
    } else {
      return(broomExtra::tidy(sw_norm))
    }
  }
}

#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param lab A character describing label for the variable. If `NULL`, variable
#'   name will be used.
#' @inheritParams ggbetweenstats
#' @inheritParams normality_message
#'
#' @importFrom rlang enquo quo_name !! as_name ensym := new_formula
#' @importFrom stats bartlett.test
#' @importFrom crayon green blue yellow red
#'
#' @inherit stats::bartlett.test return value
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @family helper_messages
#'
#' @examples
#'
#' # getting message
#' ggstatsplot::bartlett_message(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   lab = "Iris Species"
#' )
#'
#' # getting results from the test
#' ggstatsplot::bartlett_message(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   output = "tidy"
#' )
#' @export

# function body
bartlett_message <- function(data,
                             x,
                             y,
                             lab = NULL,
                             k = 2,
                             output = "message",
                             ...) {
  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # if `lab` is not provided, use the variable `x` name
  if (is.null(lab)) lab <- rlang::as_name(x)

  # running the test
  bartlett <- stats::bartlett.test(
    formula = rlang::new_formula(y, x),
    data = data,
    na.action = na.omit
  )
  p_value <- bartlett$p.value[[1]]

  # preparing message
  if (output == "message") {
    # display homogeneity of variances test result as a message
    message(cat(
      crayon::green("Note: "),
      crayon::blue("Bartlett's test for homogeneity of variances for factor "),
      crayon::yellow(lab),
      crayon::blue(": p-value = "),
      crayon::yellow(specify_decimal_p(x = p_value, k = k, p.value = TRUE)),
      "\n",
      sep = ""
    ))
  } else {
    return(broomExtra::tidy(bartlett))
  }
}

#' @title grouped_message
#' @description A note to the user about the class of the output object.
#' @noRd

# function body
grouped_message <- function() {
  message(cat(
    crayon::red("Warning: "),
    crayon::blue("Individual plots in the combined `grouped_` plot\n"),
    crayon::blue("can't be further modified with `ggplot2` functions.\n"),
    sep = ""
  ))
}

#' @title Message if palette doesn't have enough number of colors.
#' @name palette_message
#' @description A note to the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#'
#' @inheritParams paletteer::scale_fill_paletteer_d
#' @param min_length Minimum number of colors needed.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select
#' @importFrom crayon green blue yellow red
#' @importFrom rlang !! enquo
#'
#' @noRd

# function body
palette_message <- function(package, palette, min_length) {
  # computing the number of colors in a given palette
  palette_df <-
    tibble::as_tibble(paletteer::palettes_d_names) %>%
    dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
    dplyr::select(.data = ., length)

  # if insufficient number of colors are available in a given palette
  if (palette_df$length[[1]] < min_length) {
    # message to display
    message(cat(
      crayon::red("Warning: "),
      crayon::blue("No. of factor levels is greater than default palette color count.\n"),
      crayon::blue("Try using another color `palette` (and/or `package`).\n")
    ),
    sep = ""
    )
  }
}


#' @title Message to display when adjusted p-values are displayed in correlation
#'   matrix.
#' @name ggcorrmat_matrix_message
#' @noRd

# function body
ggcorrmat_matrix_message <- function() {
  message(
    cat(
      crayon::green("Note: "),
      crayon::blue("In the correlation matrix,\n"),
      crayon::blue("the upper triangle: p-values adjusted for multiple comparisons\n"),
      crayon::blue("the lower triangle: unadjusted p-values.\n"),
      sep = ""
    )
  )
}

#' @title Making expression with frequentist random-effects meta-analysis
#'   results
#' @description This analysis is carried out using the `metafor` package. For
#'   more, see `?metafor::rma`.
#' @name subtitle_meta_parametric
#'
#' @param data A dataframe. It **must** contain columns named `estimate`
#'   (corresponding estimates of coefficients or other quantities of interest)
#'   and `std.error` (the standard error of the regression term).
#' @param output  Character describing the desired output. If `"subtitle"`, a
#'   formatted subtitle with summary effect and statistical details will be
#'   returned, and if `"caption"`, expression containing details from model
#'   summary will be returned. The other option is to return `"tidy"` data frame
#'   with coefficients or `"glance"` dataframe with model summaries.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments (ignored).
#'
#' @importFrom metafor rma
#'
#' @examples
#' \donttest{
#' # let's create a dataframe
#' df_results <-
#'   structure(
#'     .Data = list(estimate = c(
#'       0.382047603321706, 0.780783111514665,
#'       0.425607573765058, 0.558365541235078, 0.956473848429961
#'     ), std.error = c(
#'       0.0465576338644502,
#'       0.0330218199731529, 0.0362834986178494, 0.0480571500648261, 0.062215818388157
#'     ), t.value = c(
#'       8.20590677855356, 23.6444603038067, 11.7300588415607,
#'       11.6187818146078, 15.3734833553524
#'     ), conf.low = c(
#'       0.290515146096969,
#'       0.715841986960399, 0.354354575031406, 0.46379116008131, 0.827446138277154
#'     ), conf.high = c(
#'       0.473580060546444, 0.845724236068931, 0.496860572498711,
#'       0.652939922388847, 1.08550155858277
#'     ), p.value = c(
#'       3.28679518728519e-15,
#'       4.04778497135963e-75, 7.59757330804449e-29, 5.45155840151592e-26,
#'       2.99171217913312e-13
#'     ), df.residual = c(
#'       394L, 358L, 622L, 298L,
#'       22L
#'     )),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' # making subtitle
#' ggstatsplot::subtitle_meta_parametric(
#'   data = df_results,
#'   k = 3,
#'   messages = FALSE
#' )
#'
#' # getting tidy data frame with coefficients
#' ggstatsplot::subtitle_meta_parametric(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "tidy"
#' )
#'
#' # making caption
#' ggstatsplot::subtitle_meta_parametric(
#'   data = df_results,
#'   k = 2,
#'   messages = FALSE,
#'   output = "caption"
#' )
#'
#' # getting dataframe with model summary
#' ggstatsplot::subtitle_meta_parametric(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "glance"
#' )
#' }
#' @export

# function body
subtitle_meta_parametric <- function(data,
                                     k = 2,
                                     messages = TRUE,
                                     output = "subtitle",
                                     caption = NULL,
                                     ...) {

  #----------------------- input checking ------------------------------------

  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe **must** contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

  #----------------------- meta-analysis ------------------------------------

  # object from meta-analysis
  meta_res <- metafor::rma(
    yi = estimate,
    sei = std.error,
    measure = "GEN",
    intercept = TRUE,
    data = data,
    vtype = "LS",
    method = "REML",
    weighted = TRUE,
    test = "z",
    level = 95,
    digits = 4,
    ...
  )

  # print the results
  if (isTRUE(messages)) print(summary(meta_res))

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coefficients
  df_tidy <-
    coef(summary(meta_res)) %>%
    tibble::as_tibble(x = .) %>%
    dplyr::rename(
      .data = .,
      std.error = se,
      z.value = zval,
      p.value = pval,
      conf.low = ci.lb,
      conf.high = ci.ub
    ) %>%
    dplyr::mutate(.data = ., term = "summary effect") %>%
    dplyr::select(
      .data = .,
      term,
      estimate,
      conf.low,
      conf.high,
      dplyr::everything()
    )

  # preparing the subtitle
  subtitle <-
    substitute(
      expr = paste(
        "Summary effect: ",
        beta,
        " = ",
        estimate,
        ", CI"["95%"],
        " [",
        LL,
        ", ",
        UL,
        "]",
        ", ",
        italic("z"),
        " = ",
        zvalue,
        ", ",
        "se = ",
        se,
        ", ",
        italic("p"),
        " = ",
        pvalue
      ),
      env = list(
        estimate = specify_decimal_p(x = df_tidy$estimate, k = k),
        LL = specify_decimal_p(x = df_tidy$conf.low, k = k),
        UL = specify_decimal_p(x = df_tidy$conf.high, k = k),
        zvalue = specify_decimal_p(x = df_tidy$z.value, k = k),
        se = specify_decimal_p(x = df_tidy$std.error, k = k),
        pvalue = specify_decimal_p(x = df_tidy$p.value, k = k, p.value = TRUE)
      )
    )

  #----------------------- model sumamry ------------------------------------

  df_glance <- with(
    data = meta_res,
    expr = tibble::tibble(
      tau2 = tau2,
      se.tau2 = se.tau2,
      k = k,
      p = p,
      m = m,
      QE = QE,
      QEp = QEp,
      QM = QM,
      QMp = QMp,
      I2 = I2,
      H2 = H2,
      int.only = int.only
    )
  )

  # preparing the subtitle
  caption <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          "Heterogeneity: ",
          italic("Q"),
          "(",
          df,
          ") = ",
          Q,
          ", ",
          italic("p"),
          " = ",
          pvalue,
          ", ",
          tau["REML"]^2,
          " = ",
          tau2,
          ", ",
          "I"^2,
          " = ",
          I2
        )
      ),
      env = list(
        top.text = caption,
        Q = specify_decimal_p(x = df_glance$QE, k = 0L),
        df = specify_decimal_p(x = (df_glance$k - 1), k = 0L),
        pvalue = specify_decimal_p(x = df_glance$QEp, k = k, p.value = TRUE),
        tau2 = specify_decimal_p(x = df_glance$tau2, k = k),
        I2 = paste(specify_decimal_p(x = df_glance$I2, k = 2L), "%", sep = "")
      )
    )

  #---------------------------- output ---------------------------------------

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "subtitle" = subtitle,
    "tidy" = df_tidy,
    "caption" = caption,
    "glance" = df_glance,
    "subtitle"
  ))
}


#' @title Bayes factor message for random-effects meta-analysis
#' @name bf_meta_message
#' @importFrom metaBMA meta_random
#'
#' @inherit metaBMA::meta_random return Description
#'
#' @inheritParams subtitle_meta_ggcoefstats
#' @inheritParams metaBMA::meta_random
#' @param d the prior distribution of the average effect size \eqn{d} specified
#'   either as the type of family (e.g., \code{"norm"}) or via
#'   \code{\link[metaBMA]{prior}}.
#' @param d.par prior parameters for \eqn{d} (only used if \code{d} specifies
#'   the type of family).
#' @param tau the prior distribution of the between-study heterogeneity
#'   \eqn{\tau} specified either as a character value (e.g.,
#'   \code{"halfcauchy"}) or via \code{\link[metaBMA]{prior}}.
#' @param tau.par prior parameters for \eqn{\tau}  (only used if \code{tau}
#'   specifies the type of family).
#' @param iter number of MCMC iterations using Stan.
#'
#' @examples
#'
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaBMA)
#'
#' # creating a dataframe
#' (df <-
#'   structure(
#'     .Data = list(
#'       study = c("1", "2", "3", "4", "5"),
#'       estimate = c(
#'         0.382047603321706,
#'         0.780783111514665,
#'         0.425607573765058,
#'         0.558365541235078,
#'         0.956473848429961
#'       ),
#'       std.error = c(
#'         0.0465576338644502,
#'         0.0330218199731529,
#'         0.0362834986178494,
#'         0.0480571500648261,
#'         0.062215818388157
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   ))
#'
#' # getting Bayes factor in favor of null hypothesis
#' ggstatsplot::bf_meta_message(
#'   data = df,
#'   k = 3,
#'   iter = 1500,
#'   messages = TRUE
#' )
#' }
#'
#' @export

# function body
bf_meta_message <- function(data,
                            k = 2,
                            d = "norm",
                            d.par = c(mean = 0, sd = 0.3),
                            tau = "halfcauchy",
                            tau.par = c(scale = 0.5),
                            iter = 10000,
                            summarize = "stan",
                            caption = NULL,
                            messages = TRUE,
                            ...) {

  #----------------------- input checking ------------------------------------

  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe **must** contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`.\n"),
      sep = ""
    )),
    call. = FALSE
    )
  }

  #----------------------- create labels column -------------------------------

  if (!"term" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., term = dplyr::row_number()) %>%
      dplyr::mutate(.data = ., term = as.character(term))
  }

  # check definition of priors for d
  # Note: "d.par" and "tau.par" are deprecated in metaBMA (>= 0.6.1)
  if (class(d) == "character") {
    d <- metaBMA::prior(family = d, param = d.par)
  } else if (!class(d) == "prior") {
    stop(
      "The argument 'd' must be ",
      "\n  (A) a character such as 'norm' specifying the family of prior distribution",
      "\n  (B) a prior distribution specified via metaBMA::prior()"
    )
  }

  # check definition of priors for tau
  if (class(tau) == "character") {
    tau <- metaBMA::prior(family = tau, param = tau.par)
  } else if (!class(tau) == "prior") {
    stop(
      "The argument 'tau' must be ",
      "\n  (A) a character such as 'halfcauchy' specifying the family of prior distribution",
      "\n  (B) a non-negative prior distribution specified via metaBMA::prior()"
    )
  }

  # extracting results from random-effects meta-analysis
  bf_meta <-
    metaBMA::meta_random(
      y = data$estimate,
      SE = data$std.error,
      labels = data$term,
      d = d,
      tau = tau,
      iter = iter,
      summarize = summarize,
      ...
    )

  # print results from meta-analysis
  if (isTRUE(messages)) print(bf_meta)

  #----------------------- preparing caption -------------------------------

  # creating a dataframe with posterior estimates
  df_estimates <-
    as.data.frame(bf_meta$estimates) %>%
    tibble::rownames_to_column(.data = ., var = "term") %>%
    tibble::as_tibble(x = .) %>%
    dplyr::filter(.data = ., term == "d")

  # prepare the bayes factor message
  bf_text <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          bf,
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          d.pmean,
          ", CI"["95%"],
          " [",
          d.pmean.LB,
          ", ",
          d.pmean.UB,
          "]"
        )
      ),
      env = list(
        top.text = caption,
        bf = specify_decimal_p(x = log(bf_meta$BF["random_H0", "random_H1"]), k = k),
        d.pmean = specify_decimal_p(x = df_estimates$mean[[1]], k = k),
        d.pmean.LB = specify_decimal_p(x = df_estimates$hpd95_lower[[1]], k = k),
        d.pmean.UB = specify_decimal_p(x = df_estimates$hpd95_upper[[1]], k = k)
      )
    )

  # return the caption
  return(bf_text)
}

#' @rdname subtitle_meta_parametric
#' @aliases subtitle_meta_parametric
#' @export

subtitle_meta_ggcoefstats <- subtitle_meta_parametric

#' @title Create labels with statistical details for `ggcoefstats`
#' @name ggcoefstats_label_maker
#'
#' @param ... Currently ignored.
#' @param tidy_df Tidy dataframe from `broomExtra::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`
#'   (default: `NULL`). This is optional argument. If provide, the `glance`
#'   summary will be used to write `caption` for the final plot.
#' @param ... Currently ignored.
#' @inheritParams ggcoefstats
#'
#' @importFrom insight is_model find_statistic
#'
#' @examples
#' \donttest{
#' # show all columns in output tibble
#' options(tibble.width = Inf)
#'
#' # for reproducibility
#' set.seed(123)
#'
#' #------------------------- models with *t*-statistic ------------------
#' # model with t-statistic
#' ggstatsplot:::ggcoefstats_label_maker(x = broomExtra::tidy(stats::lm(
#'   data = mtcars, formula = wt ~ cyl * mpg
#' )), statistic = "t")
#'
#' # (in case `x` is not a dataframe, no need to specify `statistic` argument;
#' # this will be figured out by the function itself)
#'
#' #------------------------- models with *t*-statistic ------------------
#'
#' # dataframe
#' clotting <- data.frame(
#'   u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
#'   lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
#'   lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
#' )
#'
#' # model
#' mod <-
#'   stats::glm(
#'     formula = lot1 ~ log(u),
#'     data = clotting,
#'     family = Gamma
#'   )
#'
#' # model with t-statistic
#' ggstatsplot:::ggcoefstats_label_maker(
#'   x = mod,
#'   tidy_df = broomExtra::tidy(
#'     x = mod,
#'     conf.int = TRUE,
#'     conf.level = 0.95
#'   )
#' )
#'
#' #------------------------- models with *z*-statistic --------------------
#'
#' # preparing dataframe
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' d.AD <- data.frame(treatment, outcome, counts)
#'
#' # model
#' mod <- stats::glm(
#'   formula = counts ~ outcome + treatment,
#'   family = poisson(),
#'   data = d.AD
#' )
#'
#' # creating tidy dataframe with label column
#' ggstatsplot:::ggcoefstats_label_maker(x = mod, tidy_df = broomExtra::tidy(mod))
#'
#' #------------------------- models with *f*-statistic --------------------
#' # creating a model object
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' npk.aov <- stats::aov(formula = yield ~ block + N * P * K, data = npk)
#'
#' # extracting a tidy dataframe with effect size estimate and their CIs
#' tidy_df <-
#'   ggstatsplot::lm_effsize_ci(
#'     object = npk.aov,
#'     effsize = "omega",
#'     partial = FALSE,
#'     nboot = 50
#'   ) %>%
#'   dplyr::rename(.data = ., estimate = omegasq, statistic = F.value)
#'
#' # including a new column with a label
#' ggstatsplot:::ggcoefstats_label_maker(
#'   x = npk.aov,
#'   tidy_df = tidy_df,
#'   effsize = "omega",
#'   partial = FALSE
#' )
#' }
#' @keywords internal

# function body
ggcoefstats_label_maker <- function(x,
                                    tidy_df = NULL,
                                    glance_df = NULL,
                                    statistic = NULL,
                                    k = 2,
                                    effsize = "eta",
                                    partial = TRUE,
                                    ...) {

  #----------------------- statistic cleanup ----------------------------------

  # if a dataframe
  if (isFALSE(insight::is_model(x))) {
    tidy_df <- x
  } else {
    # if not a dataframe, figure out what's the relevant statistic
    statistic <- insight::find_statistic(x)

    # standardize statistic type symbol for regression models
    statistic <-
      switch(statistic,
        "t-statistic" = "t",
        "z-statistic" = "z",
        "F-statistic" = "f"
      )
  }

  # No glance method is available for F-statistic
  if (statistic %in% c("f", "f.value", "f-value", "F-value", "F")) {
    glance_df <- NULL
  }

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "statistic",
      .funs = ~ specify_decimal_p(x = ., k = k)
    ) %>%
    signif_column(data = ., p = p.value) %>%
    p_value_formatter(df = ., k = k) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number())

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic %in% c("t", "t.value", "t-value", "T")) {
    # if `df` column is in the tidy dataframe, rename it to `df.residual`
    if ("df" %in% names(tidy_df)) {
      tidy_df %<>% dplyr::mutate(.data = ., df.residual = df)
    }

    # check if df info is available somewhere
    if ("df.residual" %in% names(glance_df) ||
      "df.residual" %in% names(tidy_df)) {

      # if glance object is available, use that `df.residual`
      if ("df.residual" %in% names(glance_df)) {
        tidy_df$df.residual <- glance_df$df.residual
      }

      # adding a new column with residual df
      tidy_df %<>%
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~italic(beta)==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "(",
                specify_decimal_p(x = .$df.residual, k = 0L),
                ")==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~italic(beta)==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic %in% c("z", "z.value", "z-value", "Z")) {
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~italic(beta)==",
              specify_decimal_p(x = .$estimate, k = k),
              ", ~italic(z)==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ")",
              sep = ""
            )
          )
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic %in% c("f", "f.value", "f-value", "F-value", "F")) {
    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(italic(eta)[p]^2))
      } else {
        tidy_df$effsize.text <- list(quote(italic(eta)^2))
      }
    }

    if (effsize == "omega") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(italic(omega)[p]^2))
      } else {
        tidy_df$effsize.text <- list(quote(italic(omega)^2))
      }
    }

    # which effect size is needed?
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              "*\",\"*",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ", ~",
              .$effsize.text,
              "==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            )
          )
      )
  }

  # unnest
  tidy_df %<>%
    tidyr::unnest(data = ., cols = c(label, data)) %>%
    dplyr::select(.data = ., -rowid)

  # return the final dataframe
  return(tibble::as_tibble(tidy_df))
}

#' @title Summary dataframe for categorical variables.
#' @name cat_label_df
#' @description Creating a dataframe with an added column corresponding to
#'   summary for categorical variables.
#'
#' @param data A dataframe containing summaries for categorical variables.
#'   Should contain columns named either `"perc"` or `"counts"` or both.
#' @param label.col.name Character that decides the column name containing
#'   summary label. This can either be `"slice.label"` (default) or
#'   `"data.label"`.
#' @param label.content Character decides what information needs to be displayed
#'   on the label in each pie or bar slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.separator If `"both"` counts and proportion information is to be
#'   displayed in a label, this argument decides whether these two pieces of
#'   information are going to be on the same line (`" "`) or on separate lines
#'   (`"\n"`).
#' @inheritParams ggpiestats
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !! :=
#'
#' @examples
#'
#' # dataframe with label column
#' ggstatsplot:::cat_label_df(
#'   data = ggstatsplot:::cat_counter(mtcars, am, cyl),
#'   label.col.name = "slice.label",
#'   label.content = "both",
#'   perc.k = 1
#' )
#' @keywords internal

# function body
cat_label_df <- function(data,
                         label.col.name = "slice.label",
                         label.content = "percentage",
                         label.separator = c("\n", " "),
                         perc.k = 1) {

  # checking what needs to be displayed in a label
  # only percentage
  if (label.content %in% c("percentage", "perc", "proportion", "prop", "%")) {
    data %<>%
      dplyr::mutate(
        .data = ., !!label.col.name := paste0(round(x = perc, digits = perc.k), "%")
      )
  }

  # only raw counts
  if (label.content %in% c("counts", "n", "count", "N")) {
    data %<>% dplyr::mutate(.data = ., !!label.col.name := paste0("n = ", counts))
  }

  # both raw counts and percentages
  if (label.content %in% c("both", "mix", "all", "everything")) {
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0(
          "n = ",
          counts,
          label.separator,
          "(",
          round(x = perc, digits = perc.k),
          "%)"
        )
      )
  }

  # return dataframe with label column
  return(data)
}


#' @title Counts and percentages across grouping variables.
#' @name cat_counter
#'
#' @param ... Additional grouping variables.
#' @inheritParams ggpiestats
#'
#' @importFrom rlang enquos !! quo_is_null ensym
#' @importFrom purrr discard
#' @importFrom dplyr select group_by summarize n arrange desc
#' @importFrom dplyr mutate mutate_at mutate_if group_by_at
#'
#' @examples
#' ggstatsplot:::cat_counter(data = ggplot2::mpg, "drv", cyl, "fl")
#' @keywords internal

# function body
cat_counter <- function(data, x, y = NULL, ...) {
  # massaging the inputs
  dots <- rlang::enquos(y, x, ..., .ignore_empty = "all")

  # discarding NULL arguments
  purrr::discard(.x = dots, .p = rlang::quo_is_null)

  # creating a dataframe with counts
  return(
    data %>%
      dplyr::group_by_at(.tbl = ., .vars = dots, .drop = TRUE) %>%
      dplyr::summarize(.data = ., counts = dplyr::n()) %>%
      dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::arrange(.data = ., dplyr::desc(!!rlang::ensym(x))) %>%
      dplyr::filter(.data = ., counts != 0L)
  )
}

#' @noRd
#' @keywords internal

# combine info about sample size plus
df_facet_label <- function(data, x, y, k = 3L) {
  data %>% {
    dplyr::full_join(
      x = cat_counter(data = ., x = {{ y }}) %>%
        dplyr::mutate(.data = ., N = paste0("(n = ", counts, ")", sep = "")),
      y = groupedstats::grouped_proptest(
        data = .,
        grouping.vars = {{ y }},
        measure = {{ x }}
      ) %>%
        dplyr::filter(.data = ., !is.na(significance)),
      by = rlang::as_name(rlang::ensym(y))
    ) %>%
      p_value_formatter(df = ., k = k) %>%
      dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~chi['gof']^2~",
              "(",
              .$parameter,
              ")==",
              specify_decimal_p(x = .$statistic, k = k),
              ", ~italic(p)",
              .$p.value.formatted,
              ")",
              sep = " "
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      ) %>%
      tidyr::unnest(data = ., c(label, data)) %>%
      dplyr::select(.data = ., -rowid, -dplyr::matches("p.value.formatted"))
  }
}


#' @noRd
#' @keywords internal

p_value_formatter <- function(df, k = 3L) {
  df %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::group_nest(.tbl = ., rowid) %>%
    dplyr::mutate(
      .data = .,
      p.value.formatted = data %>%
        purrr::map(
          .x = .,
          .f = ~ specify_decimal_p(x = .$p.value, k = k, p.value = TRUE)
        )
    ) %>%
    tidyr::unnest(data = ., cols = c(p.value.formatted, data)) %>%
    dplyr::mutate(
      .data = .,
      p.value.formatted = dplyr::case_when(
        p.value.formatted == "< 0.001" ~ "<= 0.001",
        TRUE ~ paste("==", p.value.formatted, sep = " ")
      )
    ) %>%
    dplyr::select(.data = ., -rowid)
}
