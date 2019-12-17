#' Test data from Florida Bay
#'
#' Small data set collected for positioning using acoustic telemetry and YAPS. \cr
#' The data are part of a feasibility study using YAPS on Vemco PPM style data to track fish in shallow parts of Florida Bay. Data were collected using VR2 (Vemco) hydrophones. \cr
#' Included in yaps with permission from J.S. Rehage, FIU Florida International University.
#'
#' @format A list containing 3 data.tables:
#' \describe{
#'   \item{hydros}{
#'     \itemize{
#'        \item serial Hydrophone serial number.
#'        \item x,y,z Position of hydrophones in UTM.
#'        \item sync_tag ID of co-located sync tag. Must be identical to entries in data.table detections$tag.
#'        \item idx Unique values from 1:nrow(hydros).
#'      }
#'   }
#'   \item{detections}{
#'     \itemize{
#'        \item ts Timestamp of detection in POSIXct().
#'        \item tag ID of detected tag.
#'        \item epo Timestamp as number of seconds since Unix epoch. Can be obtained using as.numeric(ts).
#'        \item frac Sub-second part of detection timestamp in fractions of second [0-1].
#'        \item serial Serial number of detecting hydrophone. Must match entry in data.table hydros.
#'      }
#'   }
#'   \item{gps}{
#'     \itemize{
#'        \item ts Timestamp of gps position in POSIXct().
#'        \item utm_x, utm_y Coordinates of position. Same projection and coordinate system as used in hydros.
#'      }
#'   }
#' }
"ssu1"

#' Test data from Lake Hald, Denmark
#'
#' 
#' 
#' @format A list containing 3 data.tables:
#' \describe{
#'   \item{hydros}{
#'     \itemize{
#'        \item serial Hydrophone serial number.
#'        \item x,y,z Position of hydrophones in UTM.
#'        \item sync_tag ID of co-located sync tag. Must be identical to entries in data.table detections$tag.
#'        \item idx Unique values from 1:nrow(hydros).
#'      }
#'   }
#'   \item{detections}{
#'     \itemize{
#'        \item ts Timestamp of detection in POSIXct().
#'        \item tag ID of detected tag.
#'        \item epo Timestamp as number of seconds since Unix epoch. Can be obtained using as.numeric(ts).
#'        \item frac Sub-second part of detection timestamp in fractions of second [0-1].
#'        \item serial Serial number of detecting hydrophone. Must match entry in data.table hydros.
#'      }
#'   }
#'   \item{gps}{
#'     \itemize{
#'        \item ts Timestamp of gps position in POSIXct().
#'        \item utm_x, utm_y Coordinates of position. Same projection and coordinate system as used in hydros.
#'      }
#'   }
#' }
"hald"