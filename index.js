const days = ['day1', 'day2', 'day3', 'day4', 'day5', 'day6', 'day7']

document.addEventListener('DOMContentLoaded', init)

function init() {
  Promise.all(days.map(getFile)).then((inputs) => {
    let app = Elm.Main.fullscreen({
      inputs: inputs
    })
  })
}

function getFile(file) {
  return new Promise((resolve, reject) => {
    let r = new XMLHttpRequest()
    r.open('GET', `/inputs/${file}.txt`, true)
    r.onreadystatechange = () => {
      if (r.readyState != 4 || r.status != 200) return
      resolve([file, r.responseText.trim()])
    }
    r.send()
  })
}
