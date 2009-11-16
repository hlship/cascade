// Copyright 2009 Howard M. Lewis Ship
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing permissions
// and limitations under the License.

// Used by exception-report view to toggle the visibility of ommitted stack frames and other
// details. Note good form: using jQuery for the call, but mapping it to $ for the duration
// of the function. This helps when jQuery is mixed with libraries like Prototype.

jQuery(function($) {
	var visible = this.checked;
	$('#omitted-toggle').click(function() {
		$('LI.c-omitted-frame, .c-omitted').toggle(visible);
	});
});
