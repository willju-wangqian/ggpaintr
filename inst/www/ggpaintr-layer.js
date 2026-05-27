// ptr_set_class custom message handler: the server toggles a CSS class on a
// DOM node by id (used for the layer-disabled / unticked-stage visual cue).
// Loaded once per page via htmltools::htmlDependency() (name
// "ggpaintr-layer"), so no self-registration guard is needed. Rides along
// inside module embeddings because it is not file-scoped to a single app.
(function(){
  Shiny.addCustomMessageHandler('ptr_set_class', function(m){
    var el = document.getElementById(m.id); if(!el) return;
    if (m.add) el.classList.add(m.cls); else el.classList.remove(m.cls);
  });
  // ADR 0025 §2 / PLAN-03: reset a fileInput's DOM state and clear the
  // Shiny input slot. shinyjs isn't a dep; the only built-in mechanism
  // is a manual DOM reset + Shiny.setInputValue() with the binding's
  // own NULL convention. Targets the fileInput's <input type="file">
  // child by Shiny id (the wrapper carries the id; the inner input is
  // the actual file picker). Also wipes the visible filename pill that
  // shiny renders as `.<id>_progress` and the input-group display
  // text so the rendered HTML no longer mentions the picked filename.
  Shiny.addCustomMessageHandler('ptr_reset_file_input', function(m){
    var wrap = document.getElementById(m.id); if(!wrap) return;
    var file = wrap.querySelector('input[type="file"]'); if(file){
      try { file.value = ''; } catch (e) { /* ignored */ }
    }
    // Bootstrap-style filename display next to the Browse... button.
    var disp = wrap.querySelector('.form-control[type="text"], .form-control.shiny-bound-input + .input-group-btn ~ input, .form-control');
    if (disp) disp.value = '';
    // Newer shiny renders the chosen filename in a span beside Browse.
    var label = wrap.querySelector('.input-group-text, .file-input-label, .form-text');
    if (label) label.textContent = '';
    Shiny.setInputValue(m.id, null, {priority: 'event'});
  });
})();
