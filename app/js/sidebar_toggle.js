console.log("hello world");

$(document).ready(function() {
  // Function to toggle sidebar and update its content based on button click
  function toggleSidebar(contentType) {
    // Hide the sidebar content by default
    $("#sidebar").removeClass("active");

    // Dynamically update content based on the selected button
    if (contentType === "sliders") {
      $("#sidebar").html(`
        <h3>Sliders Sidebar</h3>
        <p>Use the sliders below to filter the data:</p>
        <input type="range" id="recreation_potential_slider" min="0" max="1" step="0.1" value="0.5">
        <input type="range" id="species_occurrence_slider" min="0" max="1" step="0.1" value="0.5">
      `);
    } else if (contentType === "species") {
      $("#sidebar").html(`
        <h3>Species Sidebar</h3>
        <p>Select Species:</p>
        <select id="species_selector" class="selectpicker">
          <option value="species1">Species 1</option>
          <option value="species2">Species 2</option>
        </select>
      `);
    }

    // Show the sidebar content
    $("#sidebar").addClass("active");

  // Attach the toggleSidebar function to buttons
  $('#toggleSliders').click(function() {
    toggleSidebar('sliders');
  });

  $('#toggleSpecies').click(function() {
    toggleSidebar('species');
  });
});
