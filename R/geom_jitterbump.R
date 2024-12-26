sigmoid2 <- function(x_from, x_to, y_from, y_to, smooth = 5, n = 100, direction = "x", location = 0, scale = 1) {
  if(!direction %in% c("x", "y")) {stop("Only the directions x or y is allowed.")}

  if(direction == "x") {
    x <- seq(-smooth, smooth, length = n)
    y <- stats::plogis(x, location = location, scale = scale)
    out <- data.frame(x = (x + smooth) / (smooth * 2) * (x_to - x_from) + x_from,
                      y = y * (y_to - y_from) + y_from)
  }

  if(direction == "y") {
    y <- seq(-smooth, smooth, length = n)
    x <- stats::plogis(y, location = location, scale = scale)
    out <- data.frame(y = (y + smooth) / (smooth * 2) * (y_to - y_from) + y_from,
                      x = x * (x_to - x_from) + x_from)
  }
  out
}

rmlast <- function(x){
  x[-length(x)]
}

rmfirst <- function(x){
  x[-1]
}

rank_sigmoid2<- function(x, y, smooth = 8, direction = "x", scale=1, n = 100) {
  shift_width <- smooth/4

  x_ = rmfirst(x)
  y_ = rmfirst(y)
  x_lag = rmlast(x)
  y_lag = rmlast(y)
  loc = stats::runif(1, -shift_width, shift_width)

  res = mapply(sigmoid2, x_from = x_lag, x_to = x_, y_from = y_lag, y_to = y_, smooth  = smooth, direction = direction, location  = loc, scale=scale, n=n, SIMPLIFY = FALSE)
  as.data.frame(do.call("rbind", res))
}

StatJitterbump <- ggplot2::ggproto("StatBump", ggplot2::Stat,
                                   setup_data = function(data, params) {
                                     # Create x_lag, and y_lag to be passed to `compute_group`
                                     # Factors need this to be able to compute a sigmoid function
                                     data <- data %>%
                                       dplyr::mutate(r = dplyr::row_number()) %>%
                                       dplyr::arrange(x) %>%
                                       dplyr::group_by_at(vars(-PANEL, -group, -x, -y, -r)) %>%
                                       dplyr::mutate(x_lag = dplyr::lag(x),
                                                     y_lag = dplyr::lag(y)) %>%
                                       dplyr::ungroup() %>%
                                       dplyr::arrange(r) %>%
                                       dplyr::select(-r) %>%
                                       as.data.frame()
                                     data
                                   },
                                   compute_group = function(data, scales, smooth = 8, direction = "x", n_points = 100) {
                                     data <- dplyr::arrange(data, x)
                                     #data <- data[order(x),]

                                     # Handling of the special case of factors
                                     # Factors come as a df with one row
                                     if(nrow(data) == 1) {
                                       if(is.na(data$x_lag) | is.na(data$y_lag)) {
                                         return(data %>% dplyr::slice(0))
                                       } else {
                                         out <- sigmoid2(data$x_lag, data$x, data$y_lag, data$y,
                                                         smooth = smooth, direction = direction, n = n_points)
                                         return(as.data.frame(out))
                                       }
                                     }

                                     # Normal case
                                     rdf_ <- data[,!colnames(data) %in% c("x","y"), drop=FALSE]
                                     out_ <- rank_sigmoid2(data$x, data$y, smooth = smooth, direction = direction, n = n_points)
                                     out <- cbind(rdf_[rep(1:nrow(rdf_), each = nrow(out_)),],
                                                  out_[rep(1:nrow(out_), each = nrow(rdf_)),])
                                     out
                                   },

                                   required_aes = c("x", "y")
)

geom_jitterbump <- function(mapping = NULL, data = NULL, geom = "line",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           smooth = 8, direction = "x", n_points=25, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatJitterbump, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, smooth = smooth, direction = direction, n_points = n_points, ...)
  )
}
