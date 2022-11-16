#' Render a raster object
#'
#' See \code{grid::\link[grid:rasterGrob]{rasterGrob}} for details.
#'
#' @name rasterGrob
#' @rdname rasterGrob
#' @keywords internal
#' @export
#' @importFrom grid rasterGrob
#' @usage rasterGrob(image)
#' @param image Any R object that can be coerced to a raster object.
#' @return A rastergrob grob.
NULL

#' Read a bitmap image stored in the PNG format
#'
#' See \code{png::\link[png:readPNG]{readPNG}} for details.
#'
#' @name readPNG
#' @rdname readPNG
#' @keywords internal
#' @export
#' @importFrom png readPNG
#' @usage readPNG(source)
#' @param source Either name of the file to read from or a raw vector
#'   representing the PNG file content.
#' @return If native is FALSE then an array of the dimensions height x width x
#'   channels. If there is only one channel the result is a matrix. The values
#'   are reals between 0 and 1. If native is TRUE then an object of the class
#'   nativeRaster is returned instead. The latter cannot be easily computed on
#'   but is the most efficient way to draw using rasterImage.
NULL

#' Arrange multiple grobs on a page
#'
#' See \code{gridExtra::\link[gridExtra:grid.arrange]{grid.arrange}} for details.
#'
#' @name grid.arrange
#' @rdname grid.arrange
#' @keywords internal
#' @export
#' @importFrom gridExtra grid.arrange
#' @usage grid.arrange(..., ncol)
#' @param ... grobs, gtables, ggplot or trellis objects.
#' @param ncol argument of gtable.
#' @return draw on the current device.
NULL
