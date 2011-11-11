define(['jquery'], function($) {

    function doInit(moduleName, functionName, args) {
        require([moduleName], function(module) {
            module[functionName].apply(null, args);
        });
    }

    function executeInitTuple(tuple) {
        doInit(tuple[0], tuple[1], tuple[2]);
    }

    function init(initializations) {
        // Now, wait for the DOM to load before executing the initializations.
        $(function() {
            // For want of a better word, I call each one a tuple
            // which consists of a module name, a functionName, and
            // a list of arguments to the function.
            $.each(initializations, function(_, tuple) {
                executeInitTuple(tuple);
            });
        });
    }

    // In the future, there'll be additional function(s) used when handling a partial render Ajax response.
    return {
        pageInit : init
    };
});