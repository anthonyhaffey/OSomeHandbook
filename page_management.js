$(".hide_show").on("click",function(){
  var parent_div = $(this).parent().attr("id");
  var software_div = this.innerHTML.toLowerCase() + "_div";
  var div_visible = $("#" + parent_div).find("." + software_div).is(":visible");
  if(div_visible){
    $("#" + parent_div).find("." + software_div).slideUp(500);
    $(this).removeClass("btn-primary");
    $(this).addClass("btn-light");
  } else {
    $("#" + parent_div).find("." + software_div).slideDown(500);
    $(this).addClass("btn-primary");
    $(this).removeClass("btn-light");
  }
});