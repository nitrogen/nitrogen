function animate_green_head() {
     $(document).mousemove(animate_green_head_inner);
 }

function animate_green_head_inner(e) {
    var width = $(window).width();
    var height = $(window).height();
    var x = e.pageX;
    var y = e.pageY;

    var green_opacity = ((x/width) + (y/height)) / 2;
    
    $(".green_head").fadeTo(20, green_opacity);
}

animate_green_head();
    