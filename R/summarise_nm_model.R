summarise_nm_model <- function(file, model, software, rounding) {
  sum <- dplyr::bind_rows(
    sum_software(software),                    # Software name
    sum_version(model, software),              # Software version
    sum_file(file),                            # Model file
    sum_run(file),                             # Model run (model file without extension)
    sum_directory(file),                       # Model directory
    sum_reference(model, software),            # Reference model
    sum_timestart(model, software),            # Run start time
    sum_timestop(model, software),             # Run stop time
    sum_probn(model, software),                # Problem no.
    sum_label(model, software),                # Model label
    sum_description(model, software),          # Model description
    sum_input_data(model, software),           # Model input data used
    sum_nobs(model, software),                 # Number of observations
    sum_nind(model, software),                 # Number of individuals
    sum_nsim(model, software),                 # Number of simulations
    sum_simseed(model, software),              # Simulation seed
    sum_subroutine(model, software),           # Des solver
    sum_runtime(model, software),              # Estimation runtime
    sum_covtime(model, software),              # Covariance matrix runtime
    sum_term(model, software),                 # Run termination message
    sum_warnings(model, software),             # Run warnings (e.g. boundary)
    sum_errors(model, software),               # Run errors (e.g termination error)
    sum_nsig(model, software),                 # Number of significant digits
    sum_condn(model, software, rounding),      # Condition number
    sum_nesample(model, software),             # Number of esample
    sum_esampleseed(model, software),          # esample seed number
    sum_ofv(model, software),                  # Objective function value
    sum_method(model, software),               # Estimation method or sim
    sum_shk(model, software, 'eps', rounding), # Epsilon shrinkage
    sum_shk(model, software, 'eta', rounding)  # Eta shrinkage
  )
  
  # Complete missing cases for consistency
  tmp <- sum %>% 
    dplyr::filter(.$problem != 0)
  
  if (nrow(tmp) == 0) return(sum)
  
  tmp %>% 
    tidyr::complete(!!!rlang::syms(c('problem', 'label')), 
                    fill = list(subprob = 0, value = 'na')) %>%
    dplyr::bind_rows(dplyr::filter(sum, sum$problem == 0)) %>%
    dplyr::arrange_at(.vars = c('problem', 'label', 'subprob')) %>%
    dplyr::mutate(descr = dplyr::case_when(
      .$label == 'software' ~ 'Software',
      .$label == 'version' ~ 'Software version',
      .$label == 'file' ~ 'Run file',
      .$label == 'run' ~ 'Run number',
      .$label == 'dir' ~ 'Run directory',
      .$label == 'ref' ~ 'Reference model',
      .$label == 'probn' ~ 'Problem number',
      .$label == 'timestart' ~ 'Run start time',
      .$label == 'timestop' ~ 'Run stop time',
      .$label == 'descr' ~ 'Run description',
      .$label == 'label' ~ 'Run label',
      .$label == 'data' ~ 'Input data',
      .$label == 'nobs' ~ 'Number of observations',
      .$label == 'nind' ~ 'Number of individuals',
      .$label == 'nsim' ~ 'Number of simulations',
      .$label == 'simseed' ~ 'Simulation seed',
      .$label == 'subroutine' ~ 'ADVAN',
      .$label == 'runtime' ~ 'Estimation runtime',
      .$label == 'covtime' ~ 'Covariance step runtime',
      .$label == 'term' ~ 'Termination message',
      .$label == 'warnings' ~ 'Run warnings',
      .$label == 'errors' ~ 'Run errors',
      .$label == 'nsig' ~ 'Number of significant digits',
      .$label == 'condn' ~ 'Condition number',
      .$label == 'nesample' ~ 'Number of ESAMPLE',
      .$label == 'esampleseed' ~ 'ESAMPLE seed number',
      .$label == 'ofv' ~ 'Objective function value',
      .$label == 'method' ~ 'Estimation method',
      .$label == 'epsshk' ~ 'Epsilon shrinkage',
      .$label == 'etashk' ~ 'Eta shrinkage')) %>% 
    dplyr::select(dplyr::one_of('problem', 'subprob', 'descr', 'label', 'value'))
}

# Default template for function output
sum_tpl <- function(label, value) {
  dplyr::tibble(problem = 0,
                subprob = 0,
                label   = label,
                value   = value)
}

# Software name
sum_software <- function(software) {
  sum_tpl('software', software)
}

# Software version
sum_version <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$problem == 0) %>%
      dplyr::filter(stringr::str_detect(.$code, 'NONLINEAR MIXED EFFECTS MODEL PROGRAM'))
    
    if (nrow(x) == 0) return(sum_tpl('version', 'na'))
    
    sum_tpl('version', stringr::str_match(x$code, 'VERSION\\s+(.+)$')[, 2])
  }
}

# Model file name
sum_file <- function(file) {
  sum_tpl('file', basename(file))
}

# Model run name
sum_run <- function(file) {
  sum_tpl('run', update_extension(basename(file), ''))
}

# Model file directory
sum_directory <- function(file) {
  sum_tpl('dir', dirname(file))
}

# Reference model
sum_reference <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$problem == 0) %>%
      dplyr::filter(stringr::str_detect(tolower(.$comment), stringr::regex('based on\\s*:', ignore_case = TRUE)))
    
    if (nrow(x) == 0) return(sum_tpl('ref', 'na'))
    
    sum_tpl('ref', stringr::str_match(x$comment, ':\\s*(.+)$')[1, 2]) # Note: only take the first match
  }
}

# Run start time 
sum_timestart <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::slice(1) %>%
      dplyr::filter(stringr::str_detect(.$code, '\\s+\\d{2}:\\d{2}:\\d{2}\\s+'))
    
    if (nrow(x) == 0) return(sum_tpl('timestart', 'na'))
    
    sum_tpl('timestart', x$code)
  }
}

# Run stop time
sum_timestop <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::slice(nrow(model)) %>%
      dplyr::filter(stringr::str_detect(.$code, '\\s+\\d{2}:\\d{2}:\\d{2}\\s+'))
    
    if (nrow(x) == 0) return(sum_tpl('timestop', 'na'))
    
    sum_tpl('timestop', x$code)
  }
}

# Problem no.
sum_probn <- function(model, software) {
  if (software == 'nonmem') {
    x <- unique(model$problem[model$problem != 0]) 
    
    if (length(x) == 0) return(sum_tpl('probn', 'na'))
    
    dplyr::tibble(
      problem = x,
      subprob = 0,
      label   = 'probn',
      value   = as.character(x))
  }
}

# Model Label
sum_label <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'pro')
    
    if (nrow(x) == 0) return(sum_tpl('label', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'label',
                    value = as.character(.$code)) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Model description
sum_description <- function(model, software) {
  if (software == 'nonmem') {
    x <- dplyr::filter(.data = model, model$level == 0)
    start <- which(stringr::str_detect(tolower(x$comment), 
                                       stringr::regex('2.\\s*description\\s*:', 
                                                      ignore_case = TRUE)))
    if (length(start) == 1) {
      end <- which(stringr::str_detect(tolower(x$comment), '(3|x\\d)\\.\\s*\\w+'))
      end <- end[(end - start) > 0]
      end <- ifelse(length(end) == 0, nrow(x), min(end) - 1)
      
      x <- dplyr::slice(.data = x, seq(start, end)) %>% 
        {stringr::str_replace(.$comment, '^\\s*;\\s*', '')} %>% 
        stringr::str_c(collapse = ' ') %>% 
        {sum_tpl('descr', stringr::str_match(., ':\\s*(.+)$')[, 2])}
      return(value = x)
    }
    sum_tpl('descr', 'na')
  }
}

# Input data
sum_input_data <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'dat') %>% 
      dplyr::distinct(!!rlang::sym('level'), .keep_all = TRUE) # Assumes that the data is on the first row
    
    if (nrow(x) == 0) return(sum_tpl('data', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'data',
                    value = stringr::str_match(.$code, '^\\s*?([^\\s]+)\\s+')[, 2]) %>% # Note: only take the first match
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Number of observations
sum_nobs <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('TOT. NO. OF OBS RECS')))
    
    if (nrow(x) == 0) return(sum_tpl('nobs', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'nobs',
                    value = stringr::str_extract(.$code, '\\d+')) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Number of individuals
sum_nind <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('TOT. NO. OF INDIVIDUALS')))
    
    if (nrow(x) == 0) return(sum_tpl('nind', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'nind',
                    value = stringr::str_extract(.$code, '\\d+')) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Simulation number
sum_nsim <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sim') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('NSUB')))
    
    if (nrow(x) == 0) return(sum_tpl('nsim', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'nsim',
                    value = stringr::str_match(.$code, 'NSUB.*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Simulation seed
sum_simseed <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sim') %>% 
      dplyr::filter(stringr::str_detect(.$code, '\\(\\d+\\)'))
    
    if (nrow(x) == 0) return(sum_tpl('simseed', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'simseed',
                    value = stringr::str_match(.$code, '\\((\\d+)\\)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# DES solver
sum_subroutine <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'sub') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('ADVAN')))
    
    if (nrow(x) == 0) return(sum_tpl('subroutine', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'subroutine',
                    value = stringr::str_match(.$code, 'ADVAN(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Estimation runtime
sum_runtime <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, 'Elapsed estimation\\s+time'))
    
    if (nrow(x) == 0) return(sum_tpl('runtime', 'na'))
    
    x %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      dplyr::mutate(subprob = (1:n()) - 1) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(label = 'runtime',
                    value = as.ctime(stringr::str_match(.$code, '([\\.\\d]+)')[, 2])) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Covariance matrix runtime
sum_covtime <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, 'Elapsed covariance\\s+time in seconds:\\s+\\d'))
    
    if (nrow(x) == 0) return(sum_tpl('covtime', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'covtime',
                    value = as.ctime(stringr::str_match(.$code, '([\\.\\d]+)')[, 2])) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Run termination
sum_term <- function(model, software) {
  if (software == 'nonmem') {
    x <- dplyr::filter(model, model$subroutine == 'lst')
    start <- which(stringr::str_detect(x$code, stringr::fixed('0MINIMIZATION')))
    end <- which(stringr::str_detect(x$code, stringr::fixed(" NO. OF FUNCTION EVALUATIONS USED:")))
    
    if (length(start) == 0 | length(end)  == 0 | length(start)!=length(end)) return(sum_tpl('term', 'na'))
    
    x %>% 
      dplyr::slice(purrr::map2(start, end, ~seq(.x,.y)) %>% purrr::flatten_int()) %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(value = purrr::map_chr(.$data, function(y) {
        drop <- min(which(stringr::str_detect(y$code, 'NO. OF')))
        dplyr::slice(.data = y, seq(1, (drop - 1))) %>% 
          {stringr::str_trim(.$code)} %>% 
          stringr::str_trunc(width = 56) %>% 
          stringr::str_c(collapse = '\n') %>% 
          stringr::str_replace('0MINIM', 'MINIM')})) %>% 
      dplyr::mutate(subprob = 0, label = 'term') %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}


# Run warnings (e.g. boundary)
sum_warnings <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'oth') %>% 
      dplyr::filter(stringr::str_detect(.$code, 'WARNINGS AND ERRORS|\\(WARNING'))
    
    if (nrow(x) == 0) return(sum_tpl('warnings', 'na'))
    
    x %>% 
      dplyr::mutate(problem = stringr::str_match(.$code, 'FOR PROBLEM\\s+(\\d+)')[, 2]) %>% 
      tidyr::fill(!!rlang::sym('problem')) %>% 
      dplyr::mutate(problem = as.numeric(.$problem)) %>% 
      dplyr::filter(!stringr::str_detect(.$code, 'FOR PROBLEM\\s+(\\d+)')) %>% 
      dplyr::mutate(code = stringr::str_trim(.$code)) %>% 
      dplyr::mutate(code = stringr::str_trunc(.$code, width = 56)) %>% 
      dplyr::distinct(!!!rlang::syms(c('problem', 'code'))) %>%
      dplyr::group_by_at(.vars = 'problem') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(value = purrr::map_chr(.$data, ~stringr::str_c(.$code, collapse = '\n'))) %>% 
      dplyr::mutate(subprob = 0, label = 'warnings') %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Run errors (e.g termination error)
sum_errors <- function(model, software) {
  if (software == 'nonmem') {
    sum_tpl('errors', 'na') # To be added
  }
}

# Number of significant digits
sum_nsig <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('NO. OF SIG. DIGITS')))
    
    if (nrow(x) == 0) return(sum_tpl('nsig', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'nsig',
                    value = stringr::str_match(.$code, ':\\s+([\\.\\d]+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Condition number
sum_condn <- function(model, software, rounding) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::slice(which(stringr::str_detect(.$code, stringr::fixed('EIGENVALUES OF COR'))) + 4)
    
    if (nrow(x) == 0) return(sum_tpl('condn', 'na'))
    
    x %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(subprob = 0,
                    label = 'condn',
                    value = purrr::map_chr(.$data, function(x) {
                      stringr::str_trim(x$code, side = 'both') %>%  
                        stringr::str_split(pattern = '\\s+') %>% 
                        purrr::flatten_chr() %>% 
                        as.numeric() %>% 
                        {max(.)/min(.)} %>% 
                        round(digits = rounding) %>% 
                        as.character()})) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Number of ESAMPLE (i.e. NPDE)
sum_nesample <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'tab') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('ESAMPLE')))
    
    if (nrow(x) == 0) return(sum_tpl('nesample', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'nesample',
                    value = stringr::str_match(.$code, 'ESAMPLE\\s*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# ESAMPLE seed number
sum_esampleseed <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'tab') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('SEED')))
    
    if (nrow(x) == 0) return(sum_tpl('esampleseed', 'na'))
    
    x %>% 
      dplyr::mutate(subprob = 0,
                    label = 'esampleseed',
                    value = stringr::str_match(.$code, 'SEED\\s*=\\s*(\\d+)')[, 2]) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value'))
  }
}

# Objective function value
sum_ofv <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::filter(stringr::str_detect(.$code, stringr::fixed('#OBJV')))
    
    if (nrow(x) == 0) return(sum_tpl('ofv', 'na'))
    
    x %>% 
      dplyr::mutate(value = stringr::str_match(.$code, '\\*\\s+(.+)\\s+\\*')[, 2]) %>% 
      dplyr::group_by_at(.vars =  'problem') %>% 
      dplyr::mutate(subprob = (1:n()) - 1, label = 'ofv') %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}

# Estimation method or sim
sum_method <- function(model, software) {
  if (software == 'nonmem') {
    x <- model %>% 
      dplyr::filter(.$subroutine %in% c('sim', 'est')) %>% 
      dplyr::filter(stringr::str_detect(.$code, '(?<!RAN)METH|NSUB'))
    
    if (nrow(x) == 0) return(sum_tpl('method', 'na'))
    
    x %>% 
      dplyr::mutate(value = stringr::str_match(.$code, '(?<!RAN)METH[OD]*\\s*=\\s*([^\\s]+)')[, 2],
                    inter = stringr::str_detect(.$code, '\\sINTER'),
                    lapl  = stringr::str_detect(.$code, '\\sLAPLA'),
                    like  = stringr::str_detect(.$code, '\\sLIKE')) %>% 
      dplyr::mutate(value = dplyr::if_else(.$subroutine == 'sim', 'sim', .$value)) %>% 
      dplyr::mutate(value = dplyr::case_when(.$value %in% c('0', 'ZERO') ~ 'FO',
                                             .$value == '1' ~ 'FOCE',
                                             stringr::str_detect(.$value, 'COND') ~ 'FOCE',
                                             TRUE ~ tolower(.$value))) %>% 
      dplyr::mutate(value = stringr::str_c(stringr::str_to_lower(.$value), dplyr::if_else(.$inter, '-i', ''),
                                           dplyr::if_else(.$lapl, ' laplacian', ''), 
                                           dplyr::if_else(.$like, ' likelihood', ''))) %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      dplyr::mutate(subprob = (1:n()) - 1, label = 'method') %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}

# Epsilon/Eta shrinkage
sum_shk <- function(model, software, type, rounding) {
  if (software == 'nonmem') {
    # Get shrinkage from: 1) psn, 2) shr file 3) nonmem lst
    ## Method 3 (worse one)
    x <- model %>% 
      dplyr::filter(.$subroutine == 'lst') %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(start = purrr::map_int(.x = .$data, .f = function(x) {
        stringr::str_c(stringr::str_to_upper(type), 'SHRINK[^V]') %>% 
          stringr::regex(ignore_case = TRUE) %>% 
          {stringr::str_detect(string = x$code, pattern = .)} %>% 
          which() %>% 
          {ifelse(length(.) == 0, NA_integer_, .)}
      })) %>% 
      filter(!is.na(.$start))
    
    if (nrow(x) == 0) return(sum_tpl(stringr::str_c(type, 'shk'), 'na'))
    
    x <- x %>% 
      dplyr::mutate(rows = purrr::map2(.x = .$data, .y = .$start, .f = function(x, start) {
        x$code[start:nrow(x)] %>% 
          {start + (which.max(stringr::str_detect(., '^\\s+\\D')[-1]) - 1)} %>% 
          {seq(start, .)}})) %>%
      dplyr::mutate(code = purrr::map2_chr(.x = .$data, .y = .$rows, 
                                           ~stringr::str_c(.x$code[.y], collapse = ' '))) %>% 
      dplyr::mutate(code = stringr::str_match(.$code, '\\Q(%)\\E:*\\s*(.+)')[, 2]) %>% 
      dplyr::mutate(code = stringr::str_split(.$code, '\\s+')) %>% 
      dplyr::mutate(value = purrr::map(.$code, ~round(as.numeric(.), digits = rounding)),
                    grouping = purrr::map(.$code, ~stringr::str_c(' [', 1:length(.), ']', sep = ''))) %>% 
      dplyr::group_by_at(.vars = 'problem') %>% 
      dplyr::mutate(subprob = (1:n()) - 1) %>% 
      dplyr::ungroup()
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      x <- x %>% tidyr::unnest(cols = dplyr::one_of('value', 'grouping'))
    } else {
      x <- x %>% tidyr::unnest(dplyr::one_of('value', 'grouping'))
    }
    ## END TEMP
    
    x %>% 
      dplyr::filter(.$value != 100) %>% 
      dplyr::mutate(value = stringr::str_c(.$value, .$grouping)) %>% 
      dplyr::group_by_at(.vars = c('problem', 'subprob')) %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(label = stringr::str_c(type, 'shk'),
                    value = purrr::map_chr(.$data, ~stringr::str_c(.$value, collapse = ', '))) %>% 
      dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'value')) %>% 
      dplyr::ungroup()
  }
}
