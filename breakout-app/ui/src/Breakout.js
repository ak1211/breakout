"use strict";

export const showModalDialogImpl = function (target) {
	$('#' + target).modal('show')
};

export const hideModalDialogImpl = function (target) {
	$('#' + target).modal('hide')
};
