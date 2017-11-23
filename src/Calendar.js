exports.setFocus = function (elm) {
    return function () {
        elm.focus()
    }
}
