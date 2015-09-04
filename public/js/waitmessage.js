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
        if ($.cookie("downloaded")) {
            $.removeCookie("downloaded", { path: "/" });
            $('#downloaded').css('display', 'block');
            $.unblockUI;
        }
    }, 1000);
})
