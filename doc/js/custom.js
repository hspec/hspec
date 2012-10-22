$(document).ready(function() {

  // enable jquery.collapse.js plugin
  $(".example").collapse().bind("open", function(e, section) {
    section.$details.scrollintoview();
  })

  // prevent dotted outline in Firefox when example is expanded/collapsed
  $(".example-heading a").click(function () {
    $(this).blur();
  });

});
