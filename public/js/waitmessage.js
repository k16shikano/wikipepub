$(function($) {
    "use strict";
    $(document).ajaxStop($.unblockUI);

    function test() {
        $.ajax({ url: '/wikibook', cache: false });
    }

    $(document).ready(function() {
        $('#waitmessage').click(function() {
            $.blockUI({ message: "Compiling EPUB..." });
            test();
        });
    });
})
