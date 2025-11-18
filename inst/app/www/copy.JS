// www/copy.js
(function () {
  function copyToClipboard(txt) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(txt);
    }
    // Fallback for older browsers / insecure contexts
    var ta = document.createElement('textarea');
    ta.value = txt;
    ta.style.position = 'fixed';
    ta.style.top = '0';
    ta.style.left = '0';
    document.body.appendChild(ta);
    ta.focus();
    ta.select();
    let ok = false;
    try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
    document.body.removeChild(ta);
    return ok ? Promise.resolve() : Promise.reject(new Error('execCommand copy failed'));
  }

  // Receive copy requests from R
  Shiny.addCustomMessageHandler('copyText', function (message) {
    var txt = (message && message.text) ? String(message.text) : '';
    copyToClipboard(txt)
      .then(function () {
        Shiny.setInputValue('share_copy_success', Math.random(), { priority: 'event' });
      })
      .catch(function (err) {
        Shiny.setInputValue('share_copy_error', String(err), { priority: 'event' });
      });
  });
})();
