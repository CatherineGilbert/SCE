$(document).on('shiny:value', function(event) {
    setTimeout(function() {
        $(window).trigger('resize');
    }, 1000); // Increase the delay if needed
});

$(document).on('shiny:sessioninitialized', function(event) {
    $('.sidebar-toggle').on('click', function() {
        setTimeout(function() {
            $(window).trigger('resize');
        }, 250); // Adjust timing if necessary
    });
});

// Additional script to ensure heatmap plots resize correctly after rendering
$(document).on('shiny:value', function(event) {
    if (event.name === 'heatmapPlot') {
        setTimeout(function() {
            $(window).trigger('resize');
        }, 500);
    }
});