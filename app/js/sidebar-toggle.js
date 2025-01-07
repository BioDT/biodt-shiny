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
            <button id="closeSidebar" class="close-button" aria-label="Close Sidebar"><i class="fa-solid fa-x" ></i></button>
            <h3>Recreation potential</h3>
            <p>Use the sliders below to filter the data:</p>
        `);
    } else if (contentType === "species") {
        $("#sidebar").html(`
            <button id="closeSidebar" class="close-button" aria-label="Close Sidebar"><i class="fa-solid fa-x"></i></button>
            <h3>Biodiversity</h3>
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
