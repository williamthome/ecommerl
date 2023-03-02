"use strict"

/* SETUP */

function installEvents(root) {
    root.querySelectorAll("[data-event]").forEach((elem) => {
        if (!elem.dataset.eventOn) elem.dataset.eventOn = "click"
        const { event, eventOn, ...payload } = elem.dataset
        elem[`on${eventOn}`] = function() {
            // TODO: check is app is ready
            app.socket.send(event, payload)
        }
    })
}

function render(elem, html) {
    morphdom(elem, html)
    installEvents(elem)
}

document.addEventListener("DOMContentLoaded", () => {
    installEvents(document.body)
})

/* APP */

window["app"] = (() => {
    if (!("WebSocket" in window))  {
        throw new Error("App is not supported by this browser =(")
    }

    const state = {
        socket: null,
        worker: null,
        isConnected: false,
        isReady: false,
        rootElem: document.getElementById("app")
    }

    function workerSetup() {
        return new Promise((resolve) => {
            const worker = new Worker("js/worker.js")

            worker.onmessage = function (e) {
                console.log("New worker msg:", e)
                switch(e.data.event) {
                    case "render":
                        render(state.rootElem, e.data.payload)
                        break
                }
            }

            worker.onerror = function (e) {
                console.error("Worker error:", e)
            }

            state.worker = worker

            resolve()

            console.log("Worker is ready")
        })
    }

    function socketSetup() {
        return new Promise((resolve) => {
            const url = new URL(`ws://${location.host}/websocket`)

            const csrfToken =
                document
                .querySelector("meta[name='csrf-token']")
                .getAttribute("content")

            const params = [
                ["csrf_token", csrfToken],
                ["path", location.pathname],
            ]

            params.forEach(([k, v]) => url.searchParams.append(k, v))

            const socket = new WebSocket(url)

            socket.onopen = function () {
                state.isConnected = true
                document.dispatchEvent(new CustomEvent("connected"))
                !state.isReady && resolve()
                console.log("Socket is connected")
            }

            socket.onmessage = function (e) {
                const msg = e.data
                state.worker.postMessage(msg)
            }

            socket.onclose = function () {
                state.isConnected = false
                document.dispatchEvent(new CustomEvent("disconnected"))
                !state.isReady && resolve()
                console.log("Socket connection closed")
            }

            state.socket = socket

            console.log("Socket is ready")
        })
    }

    function send(event, payload = {}) {
        state.socket.send(JSON.stringify({
            event, payload
        }))
    }

    async function setup() {
        await Promise.all([
            workerSetup(),
            socketSetup(),
        ])
        state.isReady = true
    }

    return {
        setup,
        socket: {
            send,
        },
    }
})()

app.setup().then(() => {
    document.dispatchEvent(new CustomEvent("ready"))
    console.log("App is ready")
}).catch((err) => {
    console.error(err)
})
