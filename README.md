# codebase
R &amp; Python code

Setup r packages:  
```r
pkgs <- c(
)
installed_pkgs <- library()$results[,1]
uninst <- pkgs[!(pkgs %in% installed_pkgs)]
if (length(uninst) > 0) install.packages(uninst)
lapply(pkgs, library, character.only = TRUE)
```

`cut` function with pretty formatting:  
```r
cut2 <- function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, 
                  dig.lab = 3L, ordered_result = FALSE, ...) 
{
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  if (length(breaks) == 1L) {
    if (is.na(breaks) || breaks < 2L) 
      stop("invalid number of intervals")
    nb <- as.integer(breaks + 1)
    dx <- diff(rx <- range(x, na.rm = TRUE))
    if (dx == 0) {
      dx <- if (rx[1L] != 0) 
        abs(rx[1L])
      else 1
      breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
                        length.out = nb)
    }
    else {
      breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
      breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + 
                               dx/1000)
    }
  }
  else nb <- length(breaks <- sort.int(as.double(breaks)))
  if (anyDuplicated(breaks)) 
    stop("'breaks' are not unique")
  codes.only <- FALSE
  if (is.null(labels)) {
    for (dig in dig.lab:max(12L, dig.lab)) {
      ch.br <- formatC(0 + breaks, digits = dig, width = 1L, format = "f", big.mark = " ")
      if (right) {
        ch.upper <- formatC(0 + breaks, digits = dig, width = 1L, format = "f", big.mark = " ")
        ch.lower <- formatC(0 + breaks + (10 ^ -dig), digits = dig, width = 1L, format = "f", big.mark = " ")
      } else {
        ch.upper <- formatC(0 + breaks - (10 ^ -dig), digits = dig, width = 1L, format = "f", big.mark = " ")
        ch.lower <- formatC(0 + breaks, digits = dig, width = 1L, format = "f", big.mark = " ")
      }
      if (ok <- all(ch.br[-1L] != ch.br[-nb]))
        break
    }
    labels <- if (ok) 
      paste0(ch.lower[-nb], "—", ch.upper[-1L])
    
    else paste0("Range_", seq_len(nb - 1L))
    if (ok && include.lowest) {
      if (right) {
        labels[1L] <- paste0(ch.br[1L], "—", ch.upper[2L])
      } else {
        labels[nb - 1L] <- paste0(ch.lower[nb - 1L], "—", ch.br[nb])
      }
    }
  }
  else if (is.logical(labels) && !labels) 
    codes.only <- TRUE
  else if (length(labels) != nb - 1L) 
    stop("number of intervals and length of 'labels' differ")
  code <- .bincode(x, breaks, right, include.lowest)
  if (codes.only) 
    code
  else factor(code, seq_along(labels), labels, ordered = ordered_result)
}
```
