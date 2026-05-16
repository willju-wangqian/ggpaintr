// ggpaintr code mini-window behaviour: toggle from the </> icon, drag by the
// title bar, copy-to-clipboard. All DOM-local (scoped to the nearest
// .ptr-output ancestor) so several ggpaintr modules on one page work
// independently. Loaded once per page via htmltools::htmlDependency()
// (name "ggpaintr"), so no self-registration guard is needed.
(function(){
  function owner(el){return el ? el.closest('.ptr-output') : null;}
  document.addEventListener('click', function(e){
    var toggle = e.target.closest('.ptr-code-toggle');
    if (toggle){var o=owner(toggle); if(o){var w=o.querySelector('.ptr-code-window'); if(w) w.classList.toggle('ptr-open');} return;}
    var close = e.target.closest('.ptr-code-window__close');
    if (close){var w=close.closest('.ptr-code-window'); if(w) w.classList.remove('ptr-open'); return;}
    var copy = e.target.closest('.ptr-copy-btn');
    if (copy){
      var w=copy.closest('.ptr-code-window'); if(!w) return;
      var pre=w.querySelector('pre'); var txt=pre ? pre.textContent : '';
      if(!copy.dataset.orig) copy.dataset.orig = copy.textContent;
      var flash=function(msg){copy.textContent=msg; setTimeout(function(){copy.textContent=copy.dataset.orig;},1500);};
      var fallback=function(){var ok=false; try{var ta=document.createElement('textarea'); ta.value=txt; ta.style.cssText='position:fixed;top:0;left:0;opacity:0'; document.body.appendChild(ta); ta.focus(); ta.select(); ok=document.execCommand('copy'); document.body.removeChild(ta);}catch(err){} flash(ok ? 'Copied' : 'Copy failed');};
      if(navigator.clipboard && navigator.clipboard.writeText){navigator.clipboard.writeText(txt).then(function(){flash('Copied');}, fallback);} else {fallback();}
      return;
    }
  });
  document.addEventListener('pointerdown', function(e){
    var head = e.target.closest('.ptr-code-window__head');
    if (!head || e.target.closest('button')) return;
    var win = head.closest('.ptr-code-window'); if(!win) return;
    var r = win.getBoundingClientRect();
    win.style.left = r.left + 'px'; win.style.top = r.top + 'px';
    var dx = e.clientX - r.left, dy = e.clientY - r.top;
    try { head.setPointerCapture(e.pointerId); } catch(err) {}
    function move(ev){win.style.left=(ev.clientX-dx)+'px'; win.style.top=(ev.clientY-dy)+'px';}
    function up(){head.removeEventListener('pointermove',move); head.removeEventListener('pointerup',up);}
    head.addEventListener('pointermove', move); head.addEventListener('pointerup', up);
  });
})();
