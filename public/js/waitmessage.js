$(function($) {
    "use strict";
    $(document).ajaxStop($.unblockUI); 

    $(document).ready(function() {
        $('#waitmessage').click(function() {
            $.blockUI({ message: "Compiling EPUB..." });
        });
        $('#downloaded').click(function() {
            $(this).css('display', 'none');
        });
    });

    setInterval(function () {
        if ($.cookie("epubid")) {
            var epubid = $.cookie("epubid");
            $('#epub').attr("href", "wikibook-"+epubid+".epub");
            $('#downloaded').css('display', 'block');
            $.removeCookie("epubid", { path: "/"});
        }
        if ($.cookie("downloaded")) {
            $.unblockUI;
            $.removeCookie("downloaded", { path: "/" });
        }
    }, 1000);
})
