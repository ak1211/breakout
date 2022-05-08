"use strict";

exports.showModalDialogImpl = function (target) {
	$('#' + target).modal('show')
};

exports.hideModalDialogImpl = function (target) {
	$('#' + target).modal('hide')
};
