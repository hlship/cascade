// Used by exception-report view to toggle the visibility of ommitted stack frames and other
// details.
$(document).ready(function() {
	var visible = this.checked;
	$('#omitted-toggle').click(function() {
		$('LI.c-omitted-frame, .c-omitted').toggle(visible);
	});
});
