define(["jquery", "./bootstrap-twipsy", "./bootstrap-popover"],
    function($) {

        return {
            popover: function(selector, options) {
                $(selector).popover(options);
            }
        };
    });
