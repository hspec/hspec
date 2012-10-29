$(document).ready(function() {

  $(".foldable").click(function() {
    var control = $(this);
    control.toggleClass("expanded")
    control.next().slideToggle(100, function () {
      control.parent().scrollintoview();
    });
  });

  $(".foldable").each(function() {
    $(this).next().hide();
  });
});
