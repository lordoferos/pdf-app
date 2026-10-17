# PDF Toolkit

A small Shiny app for everyday PDF jobs: merge, split, extract and rotate. It runs on your own machine, so files are never uploaded anywhere.

## What it does

| Tab | What you get |
|---|---|
| **Merge** | Combine several PDFs into one. Reorder by typing file numbers, e.g. `3,1,2`. |
| **Split** | Split after specific pages, every N pages, or one page per file. Downloads as a zip. |
| **Extract / reorder** | Keep only the pages you list, in the order you list them. |
| **Rotate** | Rotate all pages or selected pages by 90°, 180° or 270° clockwise. |

### Page syntax

Used in the Extract, Split and Rotate tabs.

| You type | You get |
|---|---|
| `1-3, 7` | pages 1, 2, 3 and 7 |
| `5-1` | pages 5 to 1, reversed |
| `4-` | page 4 to the last page |

If a page doesn't exist or the input can't be read, the app shows an error instead of producing a broken file.

## Setup

You need R 4.1 or newer.

```r
install.packages(c("shiny", "qpdf", "zip"))
```

`zip` is optional. Without it, the Split tab falls back to your system's zip tool, which isn't always there on Windows.

## Run

```r
shiny::runApp("path/to/pdf_toolkit")
```

Or open `app.R` in RStudio and click **Run App**.

## Limits

- Upload limit is 200 MB per request. Change `shiny.maxRequestSize` at the top of `app.R` if you need more.
- Password-protected PDFs aren't supported. The Merge tab flags them as unreadable.
- Rotation is relative: rotating a page that is already at 90° by another 90° leaves it at 180°.

## Files

```
pdf_toolkit/
├── app.R       # UI, server and helper functions
└── README.md
```

## History

- **Oct 2026:** rebuilt after the original local copy was lost. Uses `qpdf` instead of `pdftools`. The January 2026 splitter called functions that don't exist (`split_pdf()`, `fileLink()`).
