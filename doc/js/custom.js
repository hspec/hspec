$(document).ready(function() {

  $(".foldable").click(function() {
    var control = $(this);

    control.toggleClass("expanded")
    control.text(control.hasClass("expanded") ? "hide example code" : "show example code");

    control.next().slideToggle(150, function () {
      control.parent().scrollintoview();
    });
  });

  $(".foldable").each(function() {
    var control = $(this);
    control.text("show example code");
    control.addClass("foldable-control");
    control.next().hide();
  });
});
