#' CSS customisation hooks for ggpaintr apps
#'
#' Every raw-Shiny entry point ([ptr_app()], [ptr_app_grid()],
#' [ptr_module_ui()], [ptr_shared_panel()]) accepts a `css =` argument: a character vector of
#' paths to `.css` files. Each path's parent directory is registered as a
#' Shiny resource under a hash-derived prefix, and one `<link>` tag is
#' emitted per file. User stylesheets are linked *after* `ggpaintr`'s
#' bundled stylesheet, so rules at equal specificity win.
#'
#' The bundled stylesheet exposes its theme as CSS custom properties
#' under the `.ptr-app` selector. Override any subset of these in a
#' user-supplied stylesheet to retheme without touching individual rules.
#'
#' @section Tokens:
#' \describe{
#'   \item{`--ptr-bg`}{App background. Default `#fafbfc`.}
#'   \item{`--ptr-surface`}{Card / sidebar background. Default `#ffffff`.}
#'   \item{`--ptr-border`}{Standard border colour. Default `#e7e9ee`.}
#'   \item{`--ptr-ink`}{Body text colour. Default `#1f2530`.}
#'   \item{`--ptr-muted`}{Muted / secondary text. Default `#6b7280`.}
#'   \item{`--ptr-accent`}{Primary accent (action button bg). Default `#c1372c`.}
#'   \item{`--ptr-accent-strong`}{Hover / active accent. Default `#9a2b22`.}
#'   \item{`--ptr-accent-weak`}{Faint accent tint. Default `rgba(193,55,44,0.13)`.}
#'   \item{`--ptr-accent-line`}{Accent border. Default `rgba(193,55,44,0.22)`.}
#'   \item{`--ptr-radius`}{Large radius (cards, sidebar). Default `10px`.}
#'   \item{`--ptr-radius-sm`}{Small radius (buttons, controls). Default `7px`.}
#'   \item{`--ptr-shadow`}{Card shadow. Default two-layer drop.}
#'   \item{`--ptr-shadow-lg`}{Floating-window shadow. Default two-layer drop.}
#'   \item{`--ptr-font`}{UI font stack. Default system-ui stack.}
#'   \item{`--ptr-mono`}{Mono font stack. Default system mono stack.}
#' }
#'
#' @section Specificity:
#' Some rules use a doubly-scoped selector (e.g. `.ptr-app .btn.action-button`).
#' To override these, match or exceed that specificity in your stylesheet
#' (either with the same selector, or with `!important`).
#'
#' @examples
#' \dontrun{
#'   # accent.css:
#'   #   .ptr-app { --ptr-accent: #2563eb; --ptr-accent-strong: #1e40af; }
#'   ptr_app("ggplot(mtcars, aes(var, var)) + geom_point()",
#'           css = "accent.css")
#' }
#'
#' @name ptr_css
#' @keywords internal
NULL
