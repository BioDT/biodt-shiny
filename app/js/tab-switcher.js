// tab-switcher.js
// Custom tab switching function that doesn't trigger navbar dropdown
$(document).ready(function() {
  // Make this function available to Shiny
  Shiny.addCustomMessageHandler('switchToTab', function(tabId) {
    // Find the tab element by its value attribute
    const tabLink = document.querySelector(`a[data-value="${tabId}"]`);
    
    if (tabLink) {
      // First, close any open navigation dropdowns, but preserve form input dropdowns
      $('.navbar .dropdown-menu').removeClass('show');
      $('.navbar .dropdown').removeClass('show');
      $('.navbar .nav-item.dropdown').removeClass('show');
      
      // If the tab is inside a dropdown, we need to handle it differently
      const parentDropdown = $(tabLink).closest('.dropdown-menu');
      if (parentDropdown.length) {
        // Get the parent nav item that contains this dropdown
        const navItem = parentDropdown.closest('.nav-item');
        
        // Activate the tab without opening the dropdown
        // This uses Bootstrap's tab API directly
        const tabInstance = new bootstrap.Tab(tabLink);
        tabInstance.show();
        
        // Make sure dropdown stays closed by repeatedly checking
        const ensureDropdownClosed = () => {
          // Only target navigation dropdowns, not form input dropdowns
          $('.navbar .dropdown-menu').removeClass('show');
          $('.navbar .dropdown').removeClass('show');
          $('.navbar .nav-item.dropdown').removeClass('show');
          
          // Also collapse the navbar if it's expanded on mobile
          const navbarToggler = $('.navbar-toggler');
          if (navbarToggler.length && !navbarToggler.hasClass('collapsed')) {
            navbarToggler.addClass('collapsed');
            $('.navbar-collapse').removeClass('show');
          }
        };
        
        // Run immediately and then a few more times to ensure it stays closed
        ensureDropdownClosed();
        setTimeout(ensureDropdownClosed, 50);
        setTimeout(ensureDropdownClosed, 150);
        setTimeout(ensureDropdownClosed, 300);
      } else {
        // For non-dropdown tabs, just trigger a normal click
        tabLink.click();
        
        // Also ensure navigation dropdowns are closed, but not form input dropdowns
        setTimeout(() => {
          $('.navbar .dropdown-menu').removeClass('show');
          $('.navbar .dropdown').removeClass('show');
          $('.navbar .nav-item.dropdown').removeClass('show');
        }, 50);
      }
    } else {
      console.error(`Tab with ID "${tabId}" not found`);
    }
  });
});
