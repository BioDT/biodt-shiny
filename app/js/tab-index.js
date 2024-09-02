$(document).on('shiny:connected', function() {
  // Set all nav tabs to tabindex=0 for accessibility
  $('ul.nav > li > a').attr('tabindex', '0');
});
