// app/js/fixTabindex.js
$(document).on('shiny:connected shiny:value shiny:inputchanged', () => {
  document.querySelectorAll('ul.navbar-nav > li > a').forEach((tab) => {
    tab.setAttribute('tabindex', '0'); // Main nav tabs focusable
  });

  document.querySelectorAll('ul.nav > li > a').forEach((tab) => {
    tab.setAttribute('tabindex', '0'); // Sub nav tabs focusable
  });
});

/* const observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    const attributeValue = $(mutation.target).prop(mutation.attributeName);
    if (mutation.attributeName === 'tabindex' && attributeValue === '-1') {
      $(mutation.target).prop(mutation.attributeName, '0');
    }
  });
});

$(document).ready(function() {
  $('*').each(function() {
    observer.observe(this, {
      attributes: true
    });
  });
}); */
