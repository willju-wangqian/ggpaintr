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
})();
