min_max_scaler <- function(x, feature_range=c(0,1)) {
  li = feature_range[1]
  ls = feature_range[2]
  x_std = (x - min(x))/(max(x) - min(x))
  x_scaled = x_std * (ls - li) + li
  return(x_scaled)
}