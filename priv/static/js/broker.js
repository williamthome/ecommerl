"use strict"

window["app"] = (() => {
    if (!("WebSocket" in window))  {
        throw new Error("App is not supported by this browser =(")
    }

    const state = {
        socket: null,
        worker: null,
        isConnected: false,
        isReady: false,
        rootElem: undefined,
        queue: new Map(),
    }

    async function setup() {
        await Promise.all([
            miscSetup(),
            workerSetup(),
            socketSetup(),
        ])
        state.isReady = true
    }

    function miscSetup() {
        return new Promise((resolve) => {
            document.addEventListener("DOMContentLoaded", () => {
                installEvents(document.body)
                console.log("Events installed")
            })
            resolve()
            console.log("Misc setup completed")
        })
    }

    function workerSetup() {
        return new Promise((resolve) => {
            const worker = new Worker("js/worker.js")

            worker.onmessage = function (e) {
                console.log("New worker msg:", e)
                switch(e.data.event) {
                    case "render":
                        const {id, html} = e.data.payload
                        render(id, html)
                        break
                    case "ready":
                        if (!state.isReady) resolve()
                        const {root} = e.data.payload
                        state.rootElem = document.getElementById(root)
                        console.log("Server is ready", e.data.payload)
                        break
                }
                if (e.data.payload["#id"]) {
                    const opts = state.queue.get(e.data.payload["#id"])
                    opts.onreply && opts.onreply(e.data.payload["#payload"])
                    state.queue.delete(e.data.payload["#id"])
                }
            }

            worker.onerror = function (e) {
                console.error("Worker error:", e)
            }

            state.worker = worker

            console.log("Worker is ready")
        })
    }

    // TODO: Ping to server and restart WS timeout
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

    // TODO: multiple events
    // TODO: prefix data-*, e.g. data-we??-event="foo"
    function installEvents(root) {
        root.querySelectorAll("[data-event]").forEach((elem) => {
            if (!elem.dataset.eventOn) elem.dataset.eventOn = "click"
            const { event, eventOn, ...payload } = elem.dataset
            elem[`on${eventOn}`] = function() {
                // TODO: check is app is ready
                // TODO: snake_case
                app.socket.send(event, payload)
            }
        })
    }

    function render(id, html) {
        const elem = id
            ? document.getElementById(id)
            : state.rootElem

        morphdom(elem, html)
        installEvents(elem)
        document.dispatchEvent(new CustomEvent("render", {id, html}))
    }

    function send(event, payload = {}, opts = {}) {
        if (opts.onreply || opts.onprogress) {
            const id = hashID(6)
            payload["#id"] = id
            state.queue.set(id, opts)
        }

        state.socket.send(JSON.stringify({
            event, payload
        }))
    }

    // @see https://stackoverflow.com/a/73879008/14166584
    function hashID(size) {
        const MASK = 0x3d
        const LETTERS = "abcdefghijklmnopqrstuvwxyz"
        const NUMBERS = "1234567890"
        const charset = `${NUMBERS}${LETTERS}${LETTERS.toUpperCase()}`.split("")

        const bytes = new Uint8Array(size)
        crypto.getRandomValues(bytes)

        return bytes.reduce((acc, byte) => `${acc}${charset[byte & MASK]}`, "")
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
