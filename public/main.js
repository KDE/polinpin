console.warn(JSON.parse(localStorage.getItem("storage")))

const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem("storage"))
})

app.ports.save.subscribe(storage => {
    console.warn(storage)
    localStorage.setItem('storage', JSON.stringify(storage))
})
