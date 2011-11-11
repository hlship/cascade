define(['jquery'], function($) {

    function doInit(moduleName, functionName, initArgs) {
        require([moduleName], function(module) {
            module[functionName].apply(null, [initArgs]);
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

    function doInvoke(dependencies, selector, functionName, args) {

        var targetArguments = $.makeArray(arguments).slice(3);

        require(dependencies, function() {
            var selection = $(selector);
            selection[functionName].apply(selection, targetArguments);
        });
    }

    /**
     * Invoke an arbitrary  jQuery method on a selector.
     * @param args dependencies, selector, functionName, args to pass to the function
     */
    function invoke(invokeArgs) {
        doInvoke.apply(null, invokeArgs);
    }


    return {
        pageInit : init,
        invoke : invoke
    };
});