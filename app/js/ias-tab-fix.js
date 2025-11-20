// Fix for IAS tab input binding
$(document).ready(function() {
  // Listen for custom message to set up tab listener
  Shiny.addCustomMessageHandler('ias-setup-tab-listener', function(message) {
    const tabId = message.id;
    console.log('Setting up tab listener for:', tabId);
    
    // Find all tab links for this navset
    const selector = `#${tabId} a[data-bs-toggle="tab"]`;
    $(selector).on('shown.bs.tab', function(e) {
      const tabValue = $(e.target).attr('data-value');
      console.log('IAS tab changed to:', tabValue);
      
      // Manually update the Shiny input
      Shiny.setInputValue(tabId, tabValue, {priority: 'event'});
    });
    
    // Trigger initial value
    const activeTab = $(`#${tabId} .active a[data-bs-toggle="tab"]`);
    if (activeTab.length) {
      const initialValue = activeTab.attr('data-value');
      console.log('Setting initial IAS tab value:', initialValue);
      Shiny.setInputValue(tabId, initialValue, {priority: 'event'});
    }
  });
});
