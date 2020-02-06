# 0_set_up_environment.r
#20191002

message('Setting up environment.')

options(width = 100)

conflict_prefer('filter', 'dplyr')
conflict_prefer('recode', 'dplyr')
conflict_prefer('select', 'dplyr')

get_percentage <- function(x, y, return_rounded = TRUE) {
    ratio <- x/y
    percentage <- ratio*100
    if (return_rounded) {
        return(round(percentage, 2))
    }
    else {
        return(percentage)
    }
}

get_p_from_t_test <- function(variable, group) {
    return(
        t.test(abcd_continuous[[variable]] ~ abcd_continuous[[group]]) %>% 
        tidy() %>% 
        select(c(p.value)) %>% 
        as.numeric())
}

get_p_from_fisher_test <- function(variable, group) {
    return(
        fisher.test(abcd_categorical[[variable]], abcd_categorical[[group]]) %>% 
        tidy() %>% 
        select(c(p.value)) %>% 
        as.numeric())
}

# Sources my hacked-up version of ggcoef
source(here('r', 'functions', 'my_ggcoef.r'))

message('Environment setup complete.')
