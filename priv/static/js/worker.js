self.onmessage = function (e) {
    self.postMessage(JSON.parse(e.data))
}
