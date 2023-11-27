library(ggplot2)

create_gradient_plot <- function(n, top_left, top_right, lower_left, lower_right) {
  decode_colour <- function(colour) {
    farver::decode_colour(colour, to = "oklab")[1, ]
  }
  
  get_colour <- function(a, b, mx_tl, mx_tr, mx_ll, mx_lr){
    tl <- decode_colour(top_left)
    tr <- decode_colour(top_right)
    ll <- decode_colour(lower_left)
    lr <- decode_colour(lower_right)
    
    weights <- c(mx_tl[a,b], mx_tr[a,b], mx_ll[a,b], mx_lr[a,b])
    l <- sum(weights * c(tl["l"], tr["l"], ll["l"], lr["l"])) / sum(weights)
    a <- sum(weights * c(tl["a"], tr["a"], ll["a"], lr["a"])) / sum(weights)
    b <- sum(weights * c(tl["b"], tr["b"], ll["b"], lr["b"])) / sum(weights)
    
    return (farver::encode_colour(matrix(c(l, a, b), ncol = 3), from = "oklab"))
  }
  
  x = seq(0, 1, length.out = n)
  y = seq(0, 1, length.out = n)
  
  mx_lr <- t(matrix(x, ncol = 1, nrow = n ) %*% matrix(y, ncol = n, nrow = 1))
  mx_tr <- t(matrix(rev(x), ncol = 1, nrow = n ) %*% matrix(y, ncol = n, nrow = 1))
  mx_tl <- t(matrix(rev(x), ncol = 1, nrow = n ) %*% matrix(rev(y), ncol = n, nrow = 1))
  mx_ll <- t(matrix(x, ncol = 1, nrow = n ) %*% matrix(rev(y), ncol = n, nrow = 1))
  
  df <- expand.grid(x = 1:n, y = 1:n)
  
  df$fill <- purrr::map2(df$x, df$y, get_colour, mx_tl = mx_tl, mx_tr = mx_tr, mx_ll = mx_ll, mx_lr = mx_lr)
  
  ggplot(df, aes(x,y))+
    geom_tile(aes(fill = fill))+
    scale_fill_identity()+
    scale_y_reverse()
}

create_gradient_plot(n = 64, top_left = "#a06030", top_right = "#30a060", lower_left = "#ffffff", lower_right = "#808080")
