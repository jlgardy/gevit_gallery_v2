$(function() {
  // Attach click handler to all images that have class 'chart'
  $('body').on('click', '#imageGrid td', function() {
    if ($(this).has('img.chart').length) {
      // Send data to server.
      Shiny.onInputChange("clicked", true);
      Shiny.onInputChange('code', $(this).children('img.chart').data('code'));
      
      Shiny.onInputChange('scrollPos',$(window).scrollTop());
    }
  });
  // Reset click input value when user changes tab.
  // This makes Shiny observer to observe for changes more "eagerly".
  $('body').on('click', '#opsPanel li', function() {
    Shiny.onInputChange('clicked', false);
  });
  // Add shadow to table cells that have image.
  $('body').on('DOMNodeInserted', '#imageGrid table', function() {
    $('#imageGrid td').each(function() {
      if ($(this).has('img').length) {
        $(this).addClass('shadow');
      }
    });
  });
});



// When a figure is clicked, add it to the URL hash so it can be retrieved
Shiny.addCustomMessageHandler("figClick", function(data) {
  window.location.hash = data;
});

Shiny.addCustomMessageHandler("scrollCallback",
        function(data) {
          //var scrollPos = $("#imageGrid tr:last").position().top
          //var objDiv = document.getElementById("#" + data);
          //objDiv.scrollTop = objDiv.scrollHeight;
          $(window).scrollTop(data);
          //$(window).scrollTop("#imageGrid tr:last");
        }
);


//Elevate Zoom Set Interactions
Shiny.addCustomMessageHandler("createZoom",
  function(message){
    $("#figImage_only img").elevateZoom({scrollZoom : true,tint:true, tintColour:'#F90', tintOpacity:0.5});
});

// Critically : when user selects another image, remove previous image from container
// otherwise, the image hangs around like some ghost of images past
Shiny.addCustomMessageHandler("removeZoom",
  function(message){
    $('.zoomContainer').remove();
});
