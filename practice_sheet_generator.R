# Chinese character practice sheet generator
#
# This script creates printable PDF handwriting practice sheets for Chinese
# characters. It uses square grid cells with center guide lines and can render
# full-character tracing examples from vector stroke data distributed with the
# Make Me a Hanzi project.
#
# Required packages:
#   install.packages(c("grid", "systemfonts", "jsonlite", "magick"))
#
# Expected stroke data:
#   A JSONL file such as graphics.txt from:
#   https://github.com/skishore/makemeahanzi
#
# Main entry point:
#   generate_practice_sheet()
#
# Example:
#   generate_practice_sheet(
#     texts = c("少小離家老大回", "鄉音無改鬢毛催"),
#     output_file = "traditional_chinese_practice_A4.pdf",
#     stroke_data_file = "data/graphics.txt",
#     page_orientation = "portrait",
#     rows_per_character = 2,
#     cols = 6,
#     trace_cols = 5,
#     cell_size = 92,
#     font_family = "Microsoft JhengHei"
#   )

library(grid)
library(systemfonts)
library(jsonlite)
library(magick)

# Return TRUE if the requested font family is available on the system.
font_available <- function(family) {
  if (!nzchar(family)) {
    return(FALSE)
  }

  family %in% unique(systemfonts::system_fonts()$family)
}

# Select the first installed font from a list of preferred CJK-capable fonts.
choose_font <- function(preferred = c(
  "Microsoft JhengHei",
  "PingFang TC",
  "Heiti TC",
  "Noto Sans CJK TC",
  "Noto Sans CJK SC",
  "SimHei",
  "Arial Unicode MS"
)) {
  installed <- unique(systemfonts::system_fonts()$family)
  match <- preferred[preferred %in% installed]

  if (length(match) == 0) {
    warning(
      paste(
        "No preferred CJK font was found.",
        "Character rendering depends on the default graphics device font,",
        "which may not support Chinese characters correctly."
      )
    )
    return("")
  }

  match[1]
}

# Load a JSONL character database and index records by character.
#
# The expected fields are:
#   - character: a single Han character
#   - strokes:   a character vector of SVG path strings
load_character_data <- function(file) {
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  parsed <- lapply(lines, function(line) {
    jsonlite::fromJSON(line, simplifyVector = FALSE)
  })

  chars <- vapply(
    parsed,
    function(record) {
      if (!is.null(record$character)) record$character else NA_character_
    },
    character(1)
  )

  keep <- !is.na(chars) & nzchar(chars)
  parsed <- parsed[keep]
  chars <- chars[keep]

  names(parsed) <- chars
  parsed
}

# Build an SVG document from the first n_strokes vector strokes.
#
# The Make Me a Hanzi stroke coordinates are defined in a 1024 x 1024 box.
# Padding is added to avoid clipping after rasterisation.
make_stroke_svg <- function(strokes,
                            n_strokes,
                            fill = "black",
                            alpha = 1,
                            flip_y = TRUE,
                            pad = 60) {
  n_strokes <- min(n_strokes, length(strokes))

  if (n_strokes <= 0) {
    return(NULL)
  }

  path_xml <- vapply(seq_len(n_strokes), function(i) {
    sprintf(
      '<path d="%s" fill="%s" fill-opacity="%.4f"/>',
      strokes[[i]],
      fill,
      alpha
    )
  }, character(1))

  content <- paste(path_xml, collapse = "")
  viewbox_size <- 1024 + 2 * pad

  if (flip_y) {
    content <- sprintf(
      '<g transform="translate(%d,%d) scale(1,-1)">%s</g>',
      pad,
      1024 + pad,
      content
    )
  } else {
    content <- sprintf(
      '<g transform="translate(%d,%d)">%s</g>',
      pad,
      pad,
      content
    )
  }

  sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" ',
      'width="%d" height="%d" viewBox="0 0 %d %d">%s</svg>'
    ),
    viewbox_size,
    viewbox_size,
    viewbox_size,
    viewbox_size,
    content
  )
}

# Rasterise SVG strokes with magick and draw them into the current grid page.
draw_strokes <- function(strokes,
                         x,
                         y,
                         size,
                         n_strokes = NULL,
                         fill = "black",
                         alpha = 1,
                         flip_y = TRUE,
                         y_offset = 4) {
  if (is.null(strokes) || length(strokes) == 0) {
    return(invisible(NULL))
  }

  total <- length(strokes)

  if (is.null(n_strokes)) {
    n_strokes <- total
  }

  n_strokes <- max(0, min(n_strokes, total))

  if (n_strokes == 0) {
    return(invisible(NULL))
  }

  svg_text <- make_stroke_svg(
    strokes = strokes,
    n_strokes = n_strokes,
    fill = fill,
    alpha = alpha,
    flip_y = flip_y
  )

  temp_svg <- tempfile(fileext = ".svg")
  writeLines(svg_text, temp_svg, useBytes = TRUE)

  image <- magick::image_read_svg(temp_svg)
  unlink(temp_svg)

  grid::grid.raster(
    image = as.raster(image),
    x = unit(x, "pt"),
    y = unit(y + y_offset, "pt"),
    width = unit(size, "pt"),
    height = unit(size, "pt"),
    interpolate = TRUE
  )

  invisible(NULL)
}

# Draw the outer square and center guide lines for a practice cell.
draw_guides <- function(x,
                        y,
                        size,
                        guide_col = "grey65",
                        border_col = "black",
                        guide_lwd = 0.8,
                        border_lwd = 1.2) {
  grid::grid.rect(
    x = unit(x, "pt"),
    y = unit(y, "pt"),
    width = unit(size, "pt"),
    height = unit(size, "pt"),
    gp = gpar(col = border_col, fill = NA, lwd = border_lwd)
  )

  grid::grid.lines(
    x = unit(c(x - size / 2, x + size / 2), "pt"),
    y = unit(c(y, y), "pt"),
    gp = gpar(col = guide_col, lty = "dashed", lwd = guide_lwd)
  )

  grid::grid.lines(
    x = unit(c(x, x), "pt"),
    y = unit(c(y - size / 2, y + size / 2), "pt"),
    gp = gpar(col = guide_col, lty = "dashed", lwd = guide_lwd)
  )
}

# Draw a font-based model character in the center of a cell.
draw_character <- function(char,
                           x,
                           y,
                           size,
                           font_family,
                           col = "black",
                           alpha = 1,
                           fontsize_ratio = 0.62,
                           y_offset = 4) {
  if (!nzchar(char)) {
    return(invisible(NULL))
  }

  grid::grid.text(
    label = char,
    x = unit(x, "pt"),
    y = unit(y + y_offset, "pt"),
    gp = gpar(
      fontsize = size * fontsize_ratio,
      col = col,
      alpha = alpha,
      fontfamily = font_family
    )
  )

  invisible(NULL)
}

# Draw a single cell in blank, model, or trace mode.
draw_cell <- function(x,
                      y,
                      size,
                      char = "",
                      font_family = "",
                      mode = c("blank", "model", "trace"),
                      strokes = NULL,
                      n_strokes = NULL,
                      guide_col = "grey65",
                      border_col = "black",
                      guide_lwd = 0.8,
                      border_lwd = 1.2,
                      model_col = "black",
                      trace_fill = "black",
                      trace_alpha = 0.25,
                      stroke_box_scale = 0.82,
                      fontsize_ratio = 0.62) {
  mode <- match.arg(mode)

  draw_guides(
    x = x,
    y = y,
    size = size,
    guide_col = guide_col,
    border_col = border_col,
    guide_lwd = guide_lwd,
    border_lwd = border_lwd
  )

  if (mode == "model") {
    draw_character(
      char = char,
      x = x,
      y = y,
      size = size,
      font_family = font_family,
      col = model_col,
      alpha = 1,
      fontsize_ratio = fontsize_ratio
    )
  }

  if (mode == "trace") {
    draw_strokes(
      strokes = strokes,
      x = x,
      y = y,
      size = size * stroke_box_scale,
      n_strokes = n_strokes,
      fill = trace_fill,
      alpha = trace_alpha,
      y_offset = 6
    )
  }

  invisible(NULL)
}

# Draw one row for a single character:
#   - first cell: complete traced character
#   - next trace columns: incremental stroke build-up
#   - remaining cells: blank
draw_practice_row <- function(char,
                              row_index,
                              page_height_pt,
                              total_cols = 8,
                              trace_cols = 4,
                              cell_size = 56,
                              left_margin = 18,
                              top_margin = 18,
                              row_gap = 8,
                              font_family = "",
                              char_db = NULL,
                              trace_alpha = 0.28,
                              first_cell_alpha = 1) {
  y <- page_height_pt - top_margin - cell_size / 2 -
    (row_index - 1) * (cell_size + row_gap)

  strokes <- NULL
  if (!is.null(char_db) && char %in% names(char_db)) {
    strokes <- char_db[[char]]$strokes
  }

  n_total_strokes <- if (is.null(strokes)) 0 else length(strokes)

  for (col in seq_len(total_cols)) {
    x <- left_margin + (col - 1) * cell_size + cell_size / 2

    if (col == 1) {
      draw_cell(
        x = x,
        y = y,
        size = cell_size,
        char = char,
        font_family = font_family,
        mode = "trace",
        strokes = strokes,
        n_strokes = n_total_strokes,
        trace_fill = "black",
        trace_alpha = first_cell_alpha
      )
    } else if (col <= 1 + trace_cols && col <= 1 + n_total_strokes) {
      draw_cell(
        x = x,
        y = y,
        size = cell_size,
        char = char,
        font_family = font_family,
        mode = "trace",
        strokes = strokes,
        n_strokes = col - 1,
        trace_alpha = trace_alpha
      )
    } else {
      draw_cell(
        x = x,
        y = y,
        size = cell_size,
        font_family = font_family,
        mode = "blank"
      )
    }
  }

  invisible(NULL)
}

# Draw an empty row of practice cells.
draw_blank_row <- function(row_index,
                           page_height_pt,
                           total_cols = 8,
                           cell_size = 56,
                           left_margin = 18,
                           top_margin = 18,
                           row_gap = 8,
                           font_family = "") {
  y <- page_height_pt - top_margin - cell_size / 2 -
    (row_index - 1) * (cell_size + row_gap)

  for (col in seq_len(total_cols)) {
    x <- left_margin + (col - 1) * cell_size + cell_size / 2

    draw_cell(
      x = x,
      y = y,
      size = cell_size,
      font_family = font_family,
      mode = "blank"
    )
  }

  invisible(NULL)
}

# Draw the page number at the bottom center of the page.
draw_page_number <- function(page_num,
                             page_width_pt,
                             font_family = "",
                             fontsize = 10,
                             col = "grey30") {
  grid::grid.text(
    label = as.character(page_num),
    x = unit(page_width_pt / 2, "pt"),
    y = unit(18, "pt"),
    just = c("center", "bottom"),
    gp = gpar(
      fontsize = fontsize,
      col = col,
      fontfamily = font_family
    )
  )

  invisible(NULL)
}

# Generate a PDF practice sheet from one or more input strings.
#
# Args:
#   texts:              Character vector of strings to print.
#   output_file:        Output PDF filename.
#   stroke_data_file:   JSONL file containing stroke paths.
#   page_size:          Currently only "A4" is implemented.
#   page_orientation:   "portrait" or "landscape".
#   rows_per_character: Number of rows allocated to each character.
#   cols:               Number of cells per row.
#   trace_cols:         Number of incremental trace cells after the first cell.
#   cell_size:          Cell size in points.
#   row_gap:            Gap between rows in points.
#   top_margin:         Top page margin in points.
#   bottom_margin:      Bottom page margin in points.
#   font_family:        Optional font family. If NULL, a CJK font is chosen.
#   bg:                 Page background colour.
#   trace_alpha:        Opacity for incremental trace examples.
#   first_cell_alpha:   Opacity for the first full traced character.
#
# Returns:
#   Invisibly returns the output file path.
generate_practice_sheet <- function(texts,
                                    output_file = "practice_sheet.pdf",
                                    stroke_data_file = "data/graphics.txt",
                                    page_size = "A4",
                                    page_orientation = "portrait",
                                    rows_per_character = 2,
                                    cols = 8,
                                    trace_cols = 4,
                                    cell_size = 56,
                                    row_gap = 8,
                                    top_margin = 18,
                                    bottom_margin = 18,
                                    font_family = NULL,
                                    bg = "white",
                                    trace_alpha = 0.28,
                                    first_cell_alpha = 1) {
  if (missing(texts) || length(texts) == 0) {
    stop("Provide 'texts' as a non-empty character vector.")
  }

  texts <- as.character(texts)
  texts <- texts[nzchar(trimws(texts))]

  if (length(texts) == 0) {
    stop("No non-empty strings were found in 'texts'.")
  }

  if (!is.numeric(rows_per_character) || rows_per_character < 1) {
    stop("'rows_per_character' must be a positive integer.")
  }

  if (!is.numeric(cols) || cols < 2) {
    stop("'cols' must be at least 2.")
  }

  trace_cols <- min(trace_cols, cols - 1)

  if (is.null(font_family)) {
    font_family <- choose_font()
  } else if (!font_available(font_family)) {
    stop(sprintf("Font family '%s' was not found.", font_family))
  }

  if (!file.exists(stroke_data_file)) {
    stop(
      sprintf(
        paste(
          "Character stroke data file not found: %s",
          "Use graphics.txt or another JSONL file containing",
          "'character' and 'strokes'."
        ),
        stroke_data_file
      )
    )
  }

  char_db <- load_character_data(stroke_data_file)

  if (toupper(page_size) != "A4") {
    stop("Only page_size = 'A4' is implemented in this version.")
  }

  page_width_pt <- 595.28
  page_height_pt <- 841.89

  if (tolower(page_orientation) == "landscape") {
    tmp <- page_width_pt
    page_width_pt <- page_height_pt
    page_height_pt <- tmp
  } else if (tolower(page_orientation) != "portrait") {
    stop("'page_orientation' must be either 'portrait' or 'landscape'.")
  }

  table_width_pt <- cols * cell_size
  left_margin <- (page_width_pt - table_width_pt) / 2

  if (left_margin < 0) {
    stop(
      sprintf(
        "cols = %d with cell_size = %.1f pt is too wide for A4 %s.",
        cols,
        cell_size,
        page_orientation
      )
    )
  }

  usable_height_pt <- page_height_pt - top_margin - bottom_margin
  row_height_pt <- cell_size + row_gap
  max_rows_per_page <- floor((usable_height_pt + row_gap) / row_height_pt)

  if (max_rows_per_page < rows_per_character) {
    stop(
      sprintf(
        paste(
          "At most %d row(s) fit on one page, but rows_per_character = %d.",
          "Reduce cell_size or row_gap, or use landscape orientation."
        ),
        max_rows_per_page,
        rows_per_character
      )
    )
  }

  chars_per_page <- floor(max_rows_per_page / rows_per_character)

  if (chars_per_page < 1) {
    stop("No characters can fit on a page with the current settings.")
  }

  grDevices::cairo_pdf(
    filename = output_file,
    width = page_width_pt / 72,
    height = page_height_pt / 72,
    bg = bg,
    onefile = TRUE
  )

  on.exit(grDevices::dev.off(), add = TRUE)

  page_counter <- 1

  for (text_line in texts) {
    chars <- strsplit(text_line, "", useBytes = FALSE)[[1]]
    chars <- chars[nzchar(chars)]

    if (length(chars) == 0) {
      next
    }

    split_index <- ceiling(seq_along(chars) / chars_per_page)
    pages <- split(chars, split_index)

    for (page_chars in pages) {
      grid::grid.newpage()

      pushViewport(viewport(
        x = 0,
        y = 0,
        just = c("left", "bottom"),
        width = unit(1, "npc"),
        height = unit(1, "npc"),
        xscale = c(0, page_width_pt),
        yscale = c(0, page_height_pt)
      ))

      row_counter <- 1

      for (ch in page_chars) {
        draw_practice_row(
          char = ch,
          row_index = row_counter,
          page_height_pt = page_height_pt,
          total_cols = cols,
          trace_cols = trace_cols,
          cell_size = cell_size,
          left_margin = left_margin,
          top_margin = top_margin,
          row_gap = row_gap,
          font_family = font_family,
          char_db = char_db,
          trace_alpha = trace_alpha,
          first_cell_alpha = first_cell_alpha
        )

        if (rows_per_character > 1) {
          for (k in 2:rows_per_character) {
            draw_blank_row(
              row_index = row_counter + k - 1,
              page_height_pt = page_height_pt,
              total_cols = cols,
              cell_size = cell_size,
              left_margin = left_margin,
              top_margin = top_margin,
              row_gap = row_gap,
              font_family = font_family
            )
          }
        }

        row_counter <- row_counter + rows_per_character
      }

      draw_page_number(
        page_num = page_counter,
        page_width_pt = page_width_pt,
        font_family = font_family
      )

      popViewport()
      page_counter <- page_counter + 1
    }
  }

  invisible(output_file)
}

# Example call
generate_practice_sheet(
  texts = c(
    "少小離家老大回",
    "鄉音無改鬢毛催",
    "兒童相見不相識",
    "笑問客從何處來"
  ),
  output_file = "traditional_chinese_practice_A4.pdf",
  stroke_data_file = "data/graphics.txt",
  page_size = "A4",
  page_orientation = "portrait",
  rows_per_character = 2,
  cols = 6,
  trace_cols = 5,
  cell_size = 92,
  font_family = "Microsoft JhengHei",
  first_cell_alpha = 1
)
