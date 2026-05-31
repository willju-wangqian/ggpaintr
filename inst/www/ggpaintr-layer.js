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
  // ADR 0025 item #7: the `ptr_reset_file_input` handler was removed. The
  // typed-shortcut-clears-the-fileInput behaviour is now done in R by
  // re-rendering the source uiOutput on the shortcut's rising edge
  // (ptr_setup_source_uis), which yields a fresh same-id fileInput with no
  // filename pill -- no DOM hack, and it generalises to any source widget.
})();
