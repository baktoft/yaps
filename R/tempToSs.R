#' Calculate speed of sound from water temperature, salinity and depth
#' Based on H. Medwin (1975) Speed of sound in water: A simple equation for realistic parameters. (https://doi.org/10.1121/1.380790)
#' @param temp Water temperature in degrees Celcius
#' @param sal Water slinity in parts per thousand (promille)
#' @param depth Depth in meters - default = 5 m - can typically be ignored
#' @export
tempToSs <- function (temp, sal, depth=5) 
{
    ss <- 1449.2 + 4.6 * temp - 0.055 * temp^2 + 0.00029 * temp^3 + (1.34 - 0.01 * temp) * (sal - 35) + 0.016 * depth
	
	return(ss)
}
