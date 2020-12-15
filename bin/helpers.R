calculate_interpolated_doses <- function(min_dose, max_dose, size) {
  start_value <- min_dose
  end_value <- max_dose
  if (start_value == end_value) stop("No min or max found")
  new_doses <- seq(from = start_value, to = end_value, length.out = size)
}

create_responses <- function(dose_vector, numerator = 1, add_val = 1, multiplier = 1, shift_val = 0, noise_value = 0){
  x <- dose_vector
  y <- numerator/(add_val + exp(-((multiplier * x) + shift_val)))
  if (noise_value > 0){
    y <- y + rnorm(length(y), 0, noise_value)
  }
  y
}

MC_simulation <- function(dose_vector, response_vector){
  n <- length(response_vector) - 2
  y <- NULL
  for(i in 1:n){
    y <- c(y, calculate_menger_curvature(
      dose_vector[i:(i+2)],
      response_vector[i:(i+2)])
    )
  }
  y
}

calculate_menger_curvature <- function(interpolated_dose_vector, predicted_response_vector) {
  if (length(interpolated_dose_vector) != 3 | length(predicted_response_vector) != 3) {
    stop("Need 3 data points")
  }

  # Area calculation for numerator
  matrix_temp <- cbind(interpolated_dose_vector, predicted_response_vector, 1)
  A <- 0.5 * abs(det(matrix_temp))
  numerator <- 4 * A

  # Distance calculation for denominator
  line_1 <- dist(matrix_temp[c(1, 2), -3])
  line_2 <- dist(matrix_temp[c(2, 3), -3])
  line_3 <- dist(matrix_temp[c(1, 3), -3])

  denominator <- prod(
    abs(line_1),
    abs(line_2),
    abs(line_3)
  )
  if(denominator != 0) {return(numerator/denominator)}else{0}
}

plot_doseresponse <- function(dose_vector, response_vector, curvature){
  n <- length(dose_vector) - 1
  reducer <- max(response_vector)/max(curvature)
  work <- data.frame(
    X = dose_vector[2:n],
    Y = response_vector[2:n],
    curvature = curvature * reducer
  )

  pod_id <- which.max(curvature)
  pod <- work[pod_id,]

  ggplot(work, aes(x = X)) +
    geom_line(aes(y = Y, color = "Curve Fit")) +
    geom_line(aes(y = curvature, color = "Menger Curvature")) +
    scale_y_continuous(sec.axis = sec_axis(~.*reducer, name = "Relative Curvature")) +
    geom_point(data = pod, aes(x = X, y = curvature, color = "POD")) +
    scale_color_manual(name = "", values = c("#1E88E5", "#FFC107", "black")) +
    theme_bw()

}

make_dose_response_table <- function(dose_vector, response_vector, curvature){
  curvature <- c(NA, curvature, NA)
  work <- data.frame(
    X = round(dose_vector, 4),
    Y = round(response_vector, 4),
    Curvature = round(curvature, 4),
    POD = "",
    stringsAsFactors = F
  )

  work$POD[which.max(curvature)] <- TRUE

  work
}
