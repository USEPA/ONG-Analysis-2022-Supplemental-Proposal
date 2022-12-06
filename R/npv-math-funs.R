#' npv-math-funs.R
#' These functions assist in net present value calculations.

# Formula for the sum of a geometric series
sum_of_geometric_series <- function(a, r, n) {
  
  if (! is.numeric(a) & 
      is.numeric(r) & 
      is.numeric(n)) stop("Sum of geometric series requires terms a, r, n to all be numeric")
  
  if (some(list(a, r), is.infinite)) stop("All values for the first term (a) and common ratio (r) must be finite.")
  
  if (! all(n >= 0)) stop("Geometric series cannot have a negative number of terms")
  
  case_when(
    a == 0 ~ 0,
    n == 0 ~ 0,
    
    r <= -1 & !is.finite(n) ~ NaN,
    r >= 1 & !is.finite(n) ~ Inf,
    abs(r) < 1 & !is.finite(n) ~ (a / (1 - r)), # converging infinite case
    
    r == 1 & is.finite(n) ~ a * n,
    is.finite(n) ~ (a * (1 - (r^n)) / (1 - r)), # normal case
    TRUE ~ NA_real_
  )
  
}

# Net present value of fixed periodic flows, vectorized over flows
npv_fixed_pmts <- function(pmt, n, r, first_pmt_t = 1, disc_to_t = 0) {
  
  if (! is.numeric(pmt) & 
      is.numeric(n) & 
      is.numeric(r)) stop("NPV of fixed payments requires terms a, r, n to all be numeric")
  
  if (some(list(pmt, n, r), is.infinite)) stop("All values for the terms must be finite.")
  
  if (! all(n >= 0)) stop("Must not be a negative number of periods.")
  
  common_ratios <- 1 / (1 + r)
  initial_values <- pmt * (common_ratios^(first_pmt_t - disc_to_t))
  return(sum_of_geometric_series(initial_values, common_ratios, n))
}

# Net present value of series of flows (t, pmt)
npv_xy_pmts <- function(t, pmt, r, disc_to_t = 0) {
  
  common_ratio <- 1 / (1 + r)
  
  return(pmt * common_ratio^(t - disc_to_t))
}

# Net present value of a list of (t, pmt) flow series
npv_xy_pmts_vectorized <- function(list_t, list_pmt, r, disc_to_t = 0) {
  
  npv_real <- pmap(list(list_t, list_pmt, r), 
                   function(t, pmt, r) sum(npv_xy_pmts(t, pmt, r, disc_to_t)))
}


# Calculate equivalent per-period value from present value for a vector of projects
equiv_annualized_value <- function(pv, n, r, first_pmt_t = 1) {
  
  common_ratios <- 1 / (1 + r)
  
  case_when(
    r == 1 ~ pv / n,
    
    TRUE ~ (pv / (common_ratios^first_pmt_t)) * (1 - common_ratios) / (1 - common_ratios^n)
  )
  
}

# capital recovery factor
get_crf <- function(r, life) {
  (1/(1+r))*(r*(1+r)^life)/((1+r)^life - 1)
}