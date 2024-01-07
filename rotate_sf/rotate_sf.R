#' Rotate simple features for 3D layers
#' Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualisation and works with
#' points, lines and polygons.
#'
#' @param data an object of class \code{sf}
#' @param shear_values a vector of 4 containing shear values to modify
#'     the defalut.
#' @param theta The approximate angle of rotation + or - i
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#'
#' reference
#' 
#' https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/
#' geospatial-data/#d-plots-1
#'
#' @note Slight changes were made to the referenced page.  First, added
#' ability to customize shear values.  Second, added ability to change
#' the angle in theta var Third, changed the rotate matrix equation to
#' covert the angle Fourth, used a inner product sign although this is
#' not needed due to equal size matrix multiplaction
#'
#' @note The equation for calculating the rotating angle may not be
#'     accurate or necessary but it worke for me to create an
#'     approprite rotation.
#' 
#' #' @importFrom magrittr %>%

rotate_sf <- function(data,
                      shear_values = c(2, 1.2, 0, 1),
                      theta = 4,
                      x_add = 0,
                      y_add = 0)
{
  
    shear_matrix <- function (x)
    { 
    matrix(shear_values, 2, 2) 
  }
  
    rotate_matrix <- function(x)
    { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }


    # Due to equal matrix dimensions, standard multipliccation (*) or
    # inner produce (%*%) will work (see below where inner product is
    # used)
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() %*% rotate_matrix((pi/180) * theta) + c(x_add, y_add)
    )
}
