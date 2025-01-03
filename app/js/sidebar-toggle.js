function toggleSidebar(type) {
    const sidebar = document.getElementById('sidebar');
    const buttonContainer = document.querySelector('.button-container');
    if (sidebar.classList.contains('active')) {
        sidebar.classList.remove('active');
        buttonContainer.classList.remove('moved');
    } else {
        sidebar.classList.add('active');
        buttonContainer.classList.add('moved');
    }
}

// Expose toggleSidebar and closeSidebar to the global scope
window.toggleSidebar = function(contentType) {
    $("#sidebar").removeClass("active");
    $(".button-container").removeClass("moved");

    if (contentType === "sliders") {
        $("#sidebar").html(`
            <button id="closeSidebar" class="close-button">Close</button>
            <h3>Sliders Sidebar</h3>
            <p>Use the sliders below to filter the data:</p>
            <input type="range" id="recreation_potential_slider" min="0" max="1" step="0.1" value="0.5">
            <input type="range" id="species_occurrence_slider" min="0" max="1" step="0.1" value="0.5">
        `);
    } else if (contentType === "species") {
        $("#sidebar").html(`
            <button id="closeSidebar" class="close-button">Close</button>
            <h3>Species Sidebar</h3>
            <p>Select Species:</p>
            <select id="species_selector" class="selectpicker">
                <option value="species1">Species 1</option>
                <option value="species2">Species 2</option>
            </select>
        `);

        if ($.fn.selectpicker) {
            $('.selectpicker').selectpicker();
        } else {
            console.warn("selectpicker is not defined. Skipping initialization.");
        }
    }

    $("#closeSidebar").click(function() {
        window.closeSidebar();
    });

    $("#sidebar").addClass("active");
    $(".button-container").addClass("moved");
};

// Function to close the sidebar
window.closeSidebar = function() {
    $("#sidebar").removeClass("active");
    $(".button-container").removeClass("moved");
};

// Attach button click handlers
$(document).ready(function() {
    $('#toggleSliders').click(function() {
        window.toggleSidebar('sliders');
    });

    $('#toggleSpecies').click(function() {
        window.toggleSidebar('species');
    });

    $(document).click(function(event) {
        if (!$(event.target).closest('#sidebar, .toggle-button').length) {
            window.closeSidebar();
        }
    });
});
