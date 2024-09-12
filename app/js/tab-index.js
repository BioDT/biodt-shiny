// app/js/fixTabindex.js
$(document).on('shiny:connected', function() {
  
   // Ensure all main navigation tabs are focusable
    document.querySelectorAll('ul.navbar-nav > li > a').forEach(tab => {
      tab.setAttribute('tabindex', '0');  // Main nav tabs focusable
    });

    // Ensure all sub-navigation tabs within visible sections are focusable
    document.querySelectorAll('ul.nav > li > a').forEach(tab => {
      tab.setAttribute('tabindex', '0');  // Sub nav tabs focusable
    });
  
});

 
