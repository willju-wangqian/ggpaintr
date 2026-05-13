// Widget-class-aware picker reader for ggpaintr browser audits.
//
// Why this exists: a raw `document.getElementById(id).options` read on a
// ggpaintr `var`/`colvars` picker returns 0 options in two situations
// where the widget is actually populated, which led to filing BUG-4
// (2026-05-12) and BUG-A1 (2026-05-13) as bugs when no R-side issue
// existed. The two traps:
//
//   1. shiny::selectInput(..., multiple = TRUE)  -> selectize.js moves
//      <option> children OUT of the underlying <select> and stashes them
//      on `select.selectize.options`. The native `<select>.options.length`
//      is 0 even when the picker is fully populated.
//
//   2. shinyWidgets::pickerInput()  -> bootstrap-select keeps the <option>
//      children on the inner <select>, but if the probe reads the OUTER
//      wrapper or hits a brief re-init window after a `renderUI()` swap,
//      it can read 0. Use `$el.find('option')` instead.
//
// Read the picker through `readPicker(id)`. It dispatches on the widget
// class actually attached to the element and returns the option-value
// array that genuinely reflects what the user will see in the dropdown.
//
// Paste this file's contents into a Chrome MCP `javascript_tool` call at
// the start of an audit, or `eval()` it on first use, then call
// `readPicker(id)` / `readPickerSelection(id)` per-picker.
//
// See also:
//   - dev/tasks/bug-4-browser-followup.md  (BUG-4 verification)
//   - dev/audit/feature-coverage-2026-05-13-resolution.md  (BUG-A1 verification)

(function () {
  function dispatchKind(el) {
    if (!el) return "missing";
    var $el = window.$ ? window.$(el) : null;
    if ($el && typeof $el.selectpicker === "function" && $el.data("selectpicker")) {
      return "pickerInput";
    }
    if (el.selectize) return "selectize";
    return "native";
  }

  function readPicker(id) {
    var el = document.getElementById(id);
    if (!el) return { id: id, exists: false };
    var kind = dispatchKind(el);
    var opts = [];
    var selected = null;
    if (kind === "pickerInput") {
      var $el = window.$("#" + id);
      opts = $el.find("option").toArray().map(function (o) { return o.value; });
      selected = $el.selectpicker("val");
    } else if (kind === "selectize") {
      opts = Object.keys(el.selectize.options);
      selected = el.selectize.getValue();
    } else {
      // Native <select> or wrapper around one. Reach for the inner <select>
      // if the element itself isn't one.
      var native = el.tagName === "SELECT" ? el : el.querySelector("select");
      if (native) {
        opts = Array.from(native.options).map(function (o) { return o.value; });
        selected = native.value;
      }
    }
    return { id: id, exists: true, kind: kind, opts: opts,
             nOpts: opts.length, selected: selected };
  }

  function readPickerSelection(id) {
    var r = readPicker(id);
    return r.exists ? r.selected : null;
  }

  // Convenience: read many ids at once and return a JSON-friendly array.
  function readPickers(ids) { return ids.map(readPicker); }

  window.ggpaintrAuditProbe = {
    readPicker: readPicker,
    readPickerSelection: readPickerSelection,
    readPickers: readPickers
  };
})();
