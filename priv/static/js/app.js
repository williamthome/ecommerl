"use strict"

function ping() {
    app.socket.send("ping", {}, {
        onreply: function(payload) {
            console.log("ping", payload)
        }
    })
}
