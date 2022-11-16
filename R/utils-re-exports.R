#' Render a raster object
#'
#' See \code{grid::\link[grid:rasterGrob]{rasterGrob}} for details.
#'
#' @name rasterGrob
#' @rdname rasterGrob
#' @keywords internal
#' @export
#' @importFrom grid rasterGrob
#' @usage rasterGrob(image, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#'   width = NULL, height = NULL, just = "centre", hjust = NULL, vjust = NULL,
#'   interpolate = TRUE, default.units= "npc", name = NULL, gp = gpar(),
#'   vp = NULL)
#' @param image Any R object that can be coerced to a raster object.
#' @param x A numeric vector or unit object specifying x-location.
#' @param y A numeric vector or unit object specifying y-location.
#' @param width A numeric vector or unit object specifying width.
#' @param height A numeric vector or unit object specifying height.
#' @param just The justification of the rectangle relative to its (x, y)
#'   location. If there are two values, the first value specifies horizontal
#'   justification and the second value specifies vertical justification.
#'   Possible string values are: "left", "right", "centre", "center", "bottom",
#'   and "top". For numeric values, 0 means left alignment and 1 means right
#'   alignment.
#' @param hjust A numeric vector specifying horizontal justification. If
#'   specified, overrides the just setting.
#' @param vjust A numeric vector specifying vertical justification. If
#'   specified, overrides the just setting.
#' @param default.units A string indicating the default units to use if x, y,
#'   width, or height are only given as numeric vectors.
#' @param name A character identifier.
#' @param gp An object of class "gpar", typically the output from a call to the
#'   function gpar. This is basically a list of graphical parameter settings.
#' @param vp A Grid viewport object (or NULL).
#' @param interpolate A string indicating the default units to use if x, y,
#'   width, or height are only given as numeric vectors.

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
#' @usage readPNG(source, native = FALSE, info = FALSE)
#' @param source Either name of the file to read from or a raw vector
#'   representing the PNG file content.
#' @param native determines the image representation - if FALSE (the default)
#'   then the result is an array, if TRUE then the result is a native raster
#'   representation.
#' @param info logical, if TRUE additional "info" attribute is attached to the
#'   result containing information from optional tags in the file (such as bit
#'   depth, resolution, gamma, text etc.). If the PNG file contains R metadata,
#'   it will also contain a "metadata" attribute with the unserialized R object.
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
#' @usage grid.arrange(..., newpage = TRUE)
#' @param ... grobs, gtables, ggplot or trellis objects.
#' @param newpage open a new page.
#' @return draw on the current device.
NULL
